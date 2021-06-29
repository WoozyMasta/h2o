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
import GenBashCompletions (genBashScript)
import GenFishCompletions
  ( genFishScriptSimple,
    toFishScript,
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
import Type (Command (..), Opt, Subcommand (..), asSubcommand)
import Utils (convertTabsToSpaces, infoMsg, infoTrace, mayContainsOptions, mayContainsSubcommands, warnTrace)

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
    _isPreprocessOnly :: Bool,
    _isShallowOnly :: Bool
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
          <> help "Extract CLI options from the subcommand-specific help text or man page. Enter a command-subcommand pair, like git-log, as the argument."
      )

commandInput :: Parser Input
commandInput =
  CommandInput
    <$> strOption
      ( long "command"
          <> short 'c'
          <> metavar "<string>"
          <> help "Extract CLI options from the help texts or man pages associated with the command. Subcommand pages are also scanned automatically."
      )

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "<file>"
          <> help "Extract CLI options form the text file."
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
            <*> switch
              ( long "shallow"
                  <> help "Don't scan subcommands. Applies only for file input."
              )
        )

version :: Parser ConfigOrVersion
version = flag' Version (long "version" <> help "Show version")

configOrVersion :: Parser ConfigOrVersion
configOrVersion = config <|> version

run :: ConfigOrVersion -> IO Text
run Version = return (T.concat ["h2o ", Constants.versionStr, "\n"])
run (C_ (Config input _ isExportingJSON isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly isShallowOnly))
  | isExportingJSON = infoTrace "io: JSON output" $ case input of
    (CommandInput n) -> toJSONText <$> toCommandIO n
    (SubcommandInput n subn) -> toJSONSimple (n ++ "-" ++ subn) <$> (getInputContent input =<< isBwrapAvailableIO)
    (FileInput _) ->
      if isShallowOnly
        then toJSONSimple name <$> getInputContent input False
        else toJSONText <$> (pageToCommandIO name =<< getInputContent input False)
  | isConvertingTabsToSpaces = infoTrace "io: Converting tags to spaces...\n" $ T.pack . convertTabsToSpaces 8 <$> (getInputContent input =<< isBwrapAvailableIO)
  | isListingSubcommands = infoTrace "io: Listing subcommands...\n" $ T.unlines . map T.pack <$> listSubcommandsIO name
  | isPreprocessOnly = infoTrace "io: processing (option+arg, description) splitting only" $ T.pack . formatStringPairs . preprocessAll <$> (getInputContent input =<< isBwrapAvailableIO)
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    name = case input of
      CommandInput n -> n
      SubcommandInput n _ -> n
      FileInput f -> takeBaseName f
run (C_ (Config (CommandInput name) shell _ _ _ _ _)) = toScriptFull shell <$> toCommandIO name
run (C_ (Config (SubcommandInput name subname) shell _ _ _ _ _)) =
  infoTrace (printf "io: processing subcommand-level options (%s, %s)" name subname) toScriptSimple shell cmdSubcmdName <$> optsIO
  where
    cmdSubcmdName = name ++ "-" ++ subname
    optsIO = parseMany <$> (getInputContent (SubcommandInput name subname) =<< isBwrapAvailableIO)
run (C_ (Config (FileInput f) shell _ _ _ _ isShallowOnly))
  | isShallowOnly = infoTrace "io: processing just the file" $ toScriptSimple shell name <$> optsIO
  | otherwise = infoTrace "io: processing the file and more" $ toScriptFull shell <$> (pageToCommandIO name =<< contentIO)
  where
    name = takeBaseName f
    optsIO = parseMany <$> getInputContent (FileInput f) False
    contentIO = getInputContent (FileInput f) False

getHelp :: Bool -> String -> IO String
getHelp True = infoTrace ("sandboxed" :: String) getHelpSandboxed
getHelp False = warnTrace ("No sandboxing!" :: String) getHelpBare

getHelpSub :: Bool -> String -> String -> IO String
getHelpSub True = getHelpSubSandboxed
getHelpSub False = getHelpSubBare

getHelpTemplate :: String -> [[String]] -> IO String
getHelpTemplate name argsBag = do
  xs <- mapM (fetchHelpInfo name) argsBag
  let rest = dropWhile Maybe.isNothing xs
  let result
        | null rest = ""
        | otherwise = Maybe.fromJust (head rest)
  return result

fetchHelpInfo :: FilePath -> [String] -> IO (Maybe String)
fetchHelpInfo name args = do
  (exitCode, stdout, stderr) <- Process.readProcessWithExitCode name args ""
  let res
        | isCommandNotFound name exitCode stderr = Nothing
        | not (null stdout) = Just stdout
        | mayContainsOptions stderr || mayContainsSubcommands stderr = Just stderr
        | otherwise = Nothing
  return res

isCommandNotFound :: String -> System.Exit.ExitCode -> String -> Bool
isCommandNotFound name exitCode stderr =
  exitCode == System.Exit.ExitFailure 127
    || ("bwrap: execvp " `T.append` T.pack name) `T.isPrefixOf` T.pack stderr

getHelpBare :: String -> IO String
getHelpBare name = getHelpTemplate name [["--help"], ["help"]]

getHelpSubBare :: String -> String -> IO String
getHelpSubBare name subname = getHelpTemplate name [[subname, "--help"], ["help", subname]]

bwrapArgsBase :: [String]
bwrapArgsBase = ["--ro-bind", "/", "/", "--dev", "/dev", "--tmpfs", "/tmp", "--unshare-all"]

getHelpSandboxed :: String -> IO String
getHelpSandboxed name = getHelpTemplate "bwrap" [options, altOptions]
  where
    options = bwrapArgsBase ++ [name, "--help"]
    altOptions = bwrapArgsBase ++ [name, "help"]

getHelpSubSandboxed :: String -> String -> IO String
getHelpSubSandboxed name subname = getHelpTemplate "bwrap" [options, altOptions]
  where
    options = bwrapArgsBase ++ [name, subname, "--help"]
    altOptions = bwrapArgsBase ++ [name, "help", subname]

getMan :: String -> IO String
getMan name = do
  (exitCode, stdout, _) <- Process.readCreateProcessWithExitCode cp ""
  if exitCode == System.Exit.ExitFailure 16
    then return ""
    else return stdout
  where
    s = printf "man %s | col -b" name
    cp = Process.shell s

getHelpAndMan :: Bool -> String -> IO String
getHelpAndMan isSandboxing name = do
  content <- getHelp isSandboxing name
  if null content
    then do
      content2 <- getMan name
      if null content2
        then error ("io: Neither help or man pages available: " ++ name)
        else infoTrace "io: Using manpage" $ return content2
    else infoTrace "io: Using help" $ return content

toScriptSimple :: Shell -> String -> [Opt] -> Text
toScriptSimple Fish name opts = genFishScriptSimple name opts
toScriptSimple Zsh name opts = genZshScript name opts
toScriptSimple Bash name opts = genBashScript name opts
toScriptSimple _ _ opts = T.unlines $ map (T.pack . show) opts

toScriptRootOptions :: Shell -> String -> [String] -> [Opt] -> Text
toScriptRootOptions shell name _ = toScriptSimple shell name

toScriptSubcommands :: Shell -> String -> [Command] -> Text
toScriptSubcommands _ _ subcmds = T.unlines $ map (T.pack . show . asSubcommand) subcmds

toScriptSubcommandOptions :: Shell -> String -> Command -> Text
toScriptSubcommandOptions _ name (Command subname _ opts _) =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack $ printf "(%s-%s) " name subname

getInputContent :: Input -> Bool -> IO String
getInputContent (SubcommandInput name subname) isSandboxing = getHelpSub isSandboxing name subname
getInputContent (CommandInput name) isSandboxing = getHelpAndMan isSandboxing name
getInputContent (FileInput f) _ = readFile f

toScriptFull :: Shell -> Command -> Text
toScriptFull Fish cmd = toFishScript cmd
toScriptFull shell (Command name _ rootOptions subs)
  | null subnames = warnTrace "Ignore subcommands" $ toScriptSimple shell name rootOptions
  | otherwise = T.intercalate "\n\n\n" (filter (not . T.null) entries)
  where
    subnames = map _name subs
    rootOptScript = toScriptRootOptions shell name subnames rootOptions
    subcommandScript = toScriptSubcommands shell name subs
    subcommandOptionScripts = [toScriptSubcommandOptions shell name subcmd | subcmd <- subs]
    entries = [rootOptScript, subcommandScript] ++ subcommandOptionScripts

isBwrapAvailableIO :: IO Bool
isBwrapAvailableIO = (\(e, _, _) -> e == System.Exit.ExitSuccess) <$> Process.readProcessWithExitCode "bash" ["-c", "command -v bwrap"] ""

toCommandIO :: String -> IO Command
toCommandIO name = do
  !isSandboxing <- isBwrapAvailableIO
  content <- getInputContent (CommandInput name) isSandboxing
  toCommandIOHelper name content isSandboxing

pageToCommandIO :: String -> String -> IO Command
pageToCommandIO name content = do
  !isSandboxing <- isBwrapAvailableIO
  toCommandIOHelper name content isSandboxing

toCommandIOHelper :: String -> String -> Bool -> IO Command
toCommandIOHelper name content isSandboxing = do
  subcmdOptsPairs <- subcmdOptsPairsM
  if null rootOptions && null subcmdOptsPairs
    then error ("Failed to extract information for a Command: " ++ name)
    else return $ toCommand name name rootOptions subcmdOptsPairs
  where
    -- remove duplicate _cmd in [Subcommand]
    sub2pair (Subcommand s1 s2) = (s1, s2)
    pair2sub = uncurry Subcommand
    uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair
    rootOptions = parseMany content
    subcmdCandidates =
      infoMsg "subcommand candidates : \n" $ uniqSubcommands (parseSubcommand content)
    toSubcmdOptPair sub = do
      page <- getHelpSub isSandboxing name (_cmd sub)
      let criteria = not (null page) && page /= content
      return ((sub, parseMany page), criteria)
    subcmdOptsPairsM = map fst . filter snd <$> mapM toSubcmdOptPair subcmdCandidates

-- Convert to Command given command name and text
toCommandSimple :: String -> String -> Command
toCommandSimple name content =
  if null rootOptions
    then error ("Failed to extract information for a Command: " ++ name)
    else toCommand name name rootOptions []
  where
    rootOptions = parseMany content

toJSONSimple :: String -> String -> Text
toJSONSimple name content = toJSONText (toCommandSimple name content)

listSubcommandsIO :: String -> IO [String]
listSubcommandsIO name = getSubnames <$> toCommandIO name

getSubnames :: Command -> [String]
getSubnames = map _name . _subcommands
