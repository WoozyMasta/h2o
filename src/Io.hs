{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Io where

import qualified Constants
import Data.List.Extra (stripInfix)
import qualified Data.Map.Ordered as OMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import GenBashCompletions (genBashScript)
import GenFishCompletions
  ( genFishScriptRootOptions,
    genFishScriptSimple,
    genFishScriptSubcommandOptions,
    genFishScriptSubcommands,
  )
import GenJSON (toCommand, toJSONText)
import GenZshCompletions (genZshScript)
import Layout (parseMany, preprocessAll)
import Options.Applicative
import Subcommand (parseSubcommand)
import qualified System.Exit
import System.FilePath (takeBaseName)
import qualified System.Process as Process
import Text.Printf (printf)
import Type (Command (..), Opt, Subcommand (..))
import Utils (containsOptions, convertTabsToSpaces, debugMsg)

data Input
  = CommandInput String
  | FileInput FilePath
  | SubcommandInput String String

data Config = Config
  { _input :: Input,
    _shell :: Shell,
    _isOutputJSON :: Bool,
    _isConvertingTabsToSpaces :: Bool,
    _isListingSubcommands :: Bool,
    _isPreprocessOnly :: Bool
  }

data ConfigOrVersion = Version | C_ Config

data Shell = Bash | Zsh | Fish | None deriving (Eq, Show)

toShell :: String -> Shell
toShell s
  | s' == "bash" = Bash
  | s' == "zsh" = Zsh
  | s' == "fish" = Fish
  | otherwise = None
  where
    s' = T.toLower . T.pack $ s

subcommandInput :: Parser Input
subcommandInput =
  uncurry SubcommandInput . Maybe.fromJust . stripInfix "-"
    <$> strOption
      ( long "subcommand"
          <> short 's'
          <> metavar "<string-string>"
          <> help "Specify command-subcommand pair, like git-log, to parse specific subcommand."
      )

commandInput :: Parser Input
commandInput =
  CommandInput
    <$> strOption
      ( long "command"
          <> short 'c'
          <> metavar "<string>"
          <> help "Specify command to parse. Subcommand pages are scanned automatically."
      )

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "<file>"
          <> help "Select a text file to parse."
      )

inputP :: Parser Input
inputP = commandInput <|> fileInput <|> subcommandInput

config :: Parser ConfigOrVersion
config =
  C_
    <$> ( Config
            <$> inputP
            <*> ( toShell
                    <$> strOption
                      ( long "shell"
                          <> metavar "{bash|zsh|fish|none}"
                          <> showDefault
                          <> value "none"
                          <> help "Select shell for completion script (bash|zsh|fish|none)"
                      )
                )
            <*> switch
              ( long "json"
                  <> help "Show parsed results in JSON"
              )
            <*> switch
              ( long "convert-tabs-to-spaces"
                  <> help "Convert tabs to spaces"
              )
            <*> switch
              ( long "list-subcommands"
                  <> help "List subcommands"
              )
            <*> switch
              ( long "debug"
                  <> help "Run preprocessing only (for debugging)"
              )
        )

version :: Parser ConfigOrVersion
version = flag' Version (long "version" <> help "Show version")

configOrVersion :: Parser ConfigOrVersion
configOrVersion = config <|> version

run :: ConfigOrVersion -> IO Text
run Version = return (T.concat ["h2o ", Constants.versionStr, "\n"])
run (C_ (Config input _ isExportingJSON isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly))
  | isExportingJSON = trace "[main] JSON output\n" $ writeJSON name
  | isConvertingTabsToSpaces = trace "[main] Converting tags to spaces...\n" $ T.pack . convertTabsToSpaces 8 <$> (getInputContent input =<< isBwrapAvailableIO)
  | isListingSubcommands = trace "[main] Listing subcommands...\n" $ T.unlines . map T.pack <$> listSubcommandsIO name
  | isPreprocessOnly = trace "[main] processing (option+arg, description) splitting only" $ T.pack . formatStringPairs . preprocessAll <$> (getInputContent input =<< isBwrapAvailableIO)
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    name = case input of
      CommandInput n -> n
      SubcommandInput n _ -> n
      FileInput f -> takeBaseName f
run (C_ (Config (CommandInput name) shell _ _ _ _)) = toScriptFull shell <$> toCommandIO name
run (C_ (Config (SubcommandInput name subname) shell _ _ _ _)) =
  trace "[main] processing subcommand-level options" $ toScriptSubcommandOptions shell name subname <$> optsIO
  where
    optsIO = parseMany <$> (getInputContent (SubcommandInput name subname) =<< isBwrapAvailableIO)
run (C_ (Config (FileInput f) shell _ _ _ _)) =
  trace "[main] processing options from the file" $ toScriptSimple shell name <$> optsIO
  where
    name = takeBaseName f
    optsIO = parseMany <$> getInputContent (FileInput f) False

getHelp :: Bool -> String -> IO String
getHelp True = trace "[info] sandboxed" getHelpSandboxed
getHelp False = trace "[warning] No sandboxing!" getHelp_

getHelpSub :: Bool -> String -> String -> IO String
getHelpSub True = getHelpSubSandboxed
getHelpSub False = getHelpSub_

getHelpTemplate :: String -> [String] -> [String] -> IO String
getHelpTemplate cmd options altOptions = do
  (_, stdout, stderr) <- Process.readProcessWithExitCode cmd options ""
  if null stdout
    then
      if containsOptions stderr
        then return stderr
        else (\(_, a, _) -> a) <$> Process.readProcessWithExitCode cmd altOptions ""
    else return stdout

getHelp_ :: String -> IO String
getHelp_ cmd = getHelpTemplate cmd ["--help"] ["help"]

getHelpSub_ :: String -> String -> IO String
getHelpSub_ cmd subcmd = getHelpTemplate cmd [subcmd, "--help"] ["help", subcmd]

getHelpSandboxed :: String -> IO String
getHelpSandboxed cmd = getHelpTemplate "bwrap" options altOptions
  where
    options = ["--ro-bind", "/", "/", "--dev", "/dev", "--unshare-all", cmd, "--help"]
    altOptions = ["--ro-bind", "/", "/", "--dev", "/dev", "--unshare-all", cmd, "help"]

getHelpSubSandboxed :: String -> String -> IO String
getHelpSubSandboxed cmd subcmd = getHelpTemplate "bwrap" options altOptions
  where
    options = ["--ro-bind", "/", "/", "--dev", "/dev", "--unshare-all", cmd, subcmd, "--help"]
    altOptions = ["--ro-bind", "/", "/", "--dev", "/dev", "--unshare-all", cmd, "help", subcmd]

getMan :: String -> IO String
getMan cmd =
  (\(_, a, _) -> a) <$> Process.readCreateProcessWithExitCode cp ""
  where
    s = printf "man %s | col -b" cmd
    cp = Process.shell s

getHelpAndMan :: Bool -> String -> IO String
getHelpAndMan isSandboxing cmd = do
  content <- getHelp isSandboxing cmd
  if null content
    then trace "[IO] Using manpage\n" $ getMan cmd
    else trace "[IO] Using help\n" $ return content

toScriptSimple :: Shell -> String -> [Opt] -> Text
toScriptSimple Fish cmd opts = genFishScriptSimple cmd opts
toScriptSimple Zsh cmd opts = genZshScript cmd opts
toScriptSimple Bash cmd opts = genBashScript cmd opts
toScriptSimple _ _ opts = T.unlines $ map (T.pack . show) opts

toScriptRootOptions :: Shell -> String -> [String] -> [Opt] -> Text
toScriptRootOptions Fish cmd subcmds opts = genFishScriptRootOptions cmd subcmds opts
toScriptRootOptions shell cmd _ opts = toScriptSimple shell cmd opts

toScriptSubcommands :: Shell -> String -> [Subcommand] -> Text
toScriptSubcommands Fish cmd subcmds = genFishScriptSubcommands cmd subcmds
toScriptSubcommands _ _ subcmds = T.unlines $ map (T.pack . show) subcmds

toScriptSubcommandOptions :: Shell -> String -> String -> [Opt] -> Text
toScriptSubcommandOptions Fish cmd subcmd opts = genFishScriptSubcommandOptions cmd subcmd opts
toScriptSubcommandOptions _ cmd subcmd opts =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack $ printf "(%s-%s) " cmd subcmd

getInputContent :: Input -> Bool -> IO String
getInputContent (SubcommandInput name subname) isSandboxing = getHelpSub isSandboxing name subname
getInputContent (CommandInput name) isSandboxing = getHelpAndMan isSandboxing name
getInputContent (FileInput f) _ = readFile f

toScriptFull :: Shell -> Command -> Text
toScriptFull shell (Command name _ rootOptions subs) = res
  where
    toSubcommands :: [Command] -> [Subcommand]
    toSubcommands xs = [Subcommand n desc | (Command n desc _ _) <- xs]
    subcmdNames = map _name subs
    subcmdOptsPairs = [(_name sub, _options sub) | sub <- subs]
    rootOptScript = toScriptRootOptions shell name subcmdNames rootOptions
    subcommandScript = toScriptSubcommands shell name (toSubcommands subs)
    two = [rootOptScript, subcommandScript]
    texts = map (uncurry (toScriptSubcommandOptions shell name)) subcmdOptsPairs
    res =
      if null subcmdNames
        then trace "[warning] Ignore subcommands" $ toScriptSimple shell name rootOptions
        else T.intercalate "\n\n\n" (two ++ texts)

isBwrapAvailableIO :: IO Bool
isBwrapAvailableIO = (\(e, _, _) -> e == System.Exit.ExitSuccess) <$> Process.readProcessWithExitCode "bash" ["-c", "command -v bwrap"] ""

toCommandIO :: String -> IO Command
toCommandIO cmd = do
  !isSandboxing <- isBwrapAvailableIO
  rootContent <- getInputContent (CommandInput cmd) isSandboxing
  let rootOptions = parseMany rootContent
  let subcmdCandidates =
        debugMsg "subcommand candidates : \n" $ filterSubcmds (parseSubcommand rootContent)
  let toSubcmdOptPair sub = do
        page <- getHelpSub isSandboxing cmd (_cmd sub)
        let criteria = not (null page) && page /= rootContent
        return ((sub, parseMany page), criteria)
  let subcmdOptsPairsM = map fst . filter snd <$> mapM toSubcmdOptPair subcmdCandidates
  toCommand cmd cmd rootOptions <$> subcmdOptsPairsM
  where
    -- remove duplicate _cmd in [Subcommand]
    sub2pair (Subcommand s1 s2) = (s1, s2)
    pair2sub = uncurry Subcommand
    filterSubcmds = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair

-- | Generate shell completion script from the root help page
-- as well as the subcommand's help pages.
writeJSON :: String -> IO Text
writeJSON s = toJSONText <$> toCommandIO s

listSubcommandsIO :: String -> IO [String]
listSubcommandsIO s = getSubnames <$> toCommandIO s

getSubnames :: Command -> [String]
getSubnames = map _name . _subcommands
