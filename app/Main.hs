{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (filterM, (<=<))
import Data.List.Extra (nubSort, stripInfix)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
import System.FilePath (takeBaseName)
import System.Process (createProcess, readProcess)
import qualified System.Process as Process
import Text.Printf (printf)
import Type (Command, Opt, Subcommand (..))
import Utils (convertTabsToSpaces)

data Input
  = SubcommandInput String String
  | CommandInput String
  | FileInput FilePath

data Config = Config
  { _input :: Input,
    _shell :: Shell,
    _isOutputJSON :: Bool,
    _isConvertingTabsToSpaces :: Bool,
    _isListingSubcommands :: Bool,
    _isPreprocessOnly :: Bool
  }

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
          <> metavar "<STRING>"
          <> help "Specify command-subcommand name to parse"
      )

commandInput :: Parser Input
commandInput =
  CommandInput
    <$> strOption
      ( long "command"
          <> short 'c'
          <> metavar "<STRING>"
          <> help "Specify command to parse."
      )

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "<FILE>"
          <> help "Select a text file to parse"
      )

inputP :: Parser Input
inputP = subcommandInput <|> commandInput <|> fileInput

config :: Parser Config
config =
  Config
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
          <> help "Output in JSON"
      )
    <*> switch
      ( long "convert-tabs-to-spaces"
          <> help "Convert tabs to spaces"
      )
    <*> switch
      ( long "list-subcommands"
          <> help "List subcommands (experimental)"
      )
    <*> switch
      ( long "debug"
          <> help "Run preprocessing only"
      )

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "Parse help or manpage texts, extract command options, and generate shell completion scripts"
        )

run :: Config -> IO ()
run (Config (CommandInput name) _ True _ _ _) = trace "[main] JSON output\n" $ writeJSON name
run (Config input shell _ isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly) = do
  content <- getInputContent input
  let opts = parseMany content
  let res
        | isConvertingTabsToSpaces =
          trace "[main] Converting tags to spaces...\n" $
            putStr . convertTabsToSpaces 8 $ content
        | isPreprocessOnly =
          trace "[main] processing (option+arg, description) splitting only" $
            putStr . formatStringPairs . preprocessAll $ content
        | isListingSubcommands =
          trace "[main] Listing subcommands...\n" $
            putStr . unlines =<< listSubcommandsIO cmd
        | otherwise =
          case input of
            SubcommandInput _ subname ->
              trace "[main] processing subcommand-level options" $
                TIO.putStr (genScriptSubcommandOptions shell cmd subname opts)
            CommandInput name ->
              TIO.putStr =<< toScriptFull shell name
            FileInput _ ->
              trace "[main] processing options from the file" $
                TIO.putStr (genScriptSimple shell cmd opts)
  res
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    cmd = case input of
      SubcommandInput name _ -> name
      CommandInput name -> name
      FileInput f -> takeBaseName f

getHelp :: String -> IO String
getHelp cmd = do
  content <- readProcess cmd ["--help"] ""
  if null content
    then readProcess cmd ["help"] ""
    else return content

getHelpSub :: String -> String -> IO String
getHelpSub cmd subcmd = do
  content <- readProcess cmd [subcmd, "--help"] ""
  if null content
    then readProcess cmd ["help", subcmd] "" -- samtools
    else return content

readProcessAsText :: String -> [String] -> IO Text
readProcessAsText cmd args = do
  (_, houtMay, _, _) <- createProcess (Process.proc cmd args) {Process.std_out = Process.CreatePipe}
  case houtMay of
    Just hout -> TIO.hGetContents hout
    Nothing -> return T.empty

getHelpAsText :: String -> IO Text
getHelpAsText cmd = do
  content <- readProcessAsText cmd ["--help"]
  if T.null content
    then readProcessAsText cmd ["help"]
    else return content

getHelpSubAsText :: String -> String -> IO Text
getHelpSubAsText cmd subcmd = do
  content <- readProcessAsText cmd [subcmd, "--help"]
  if T.null content
    then readProcessAsText cmd ["help", subcmd] -- samtools
    else return content

isSub :: String -> String -> IO Bool
isSub cmd subcmd = do
  content <- getHelpSubAsText cmd subcmd
  contentRoot <- getHelpAsText cmd
  return $ not (T.null content) && (content /= contentRoot)

genScriptSimple :: Shell -> String -> [Opt] -> Text
genScriptSimple Fish cmd opts = genFishScriptSimple cmd opts
genScriptSimple Zsh cmd opts = genZshScript cmd opts
genScriptSimple Bash cmd opts = genBashScript cmd opts
genScriptSimple _ _ opts = T.unlines $ map (T.pack . show) opts

genScriptRootOptions :: Shell -> String -> [String] -> [Opt] -> Text
genScriptRootOptions Fish cmd subcmds opts = genFishScriptRootOptions cmd subcmds opts
genScriptRootOptions shell cmd _ opts = genScriptSimple shell cmd opts

genScriptSubcommands :: Shell -> String -> [Subcommand] -> Text
genScriptSubcommands Fish cmd subcmds = genFishScriptSubcommands cmd subcmds
genScriptSubcommands _ _ subcmds = T.unlines $ map (T.pack . show) subcmds

genScriptSubcommandOptions :: Shell -> String -> String -> [Opt] -> Text
genScriptSubcommandOptions Fish cmd subcmd opts = genFishScriptSubcommandOptions cmd subcmd opts
genScriptSubcommandOptions _ cmd subcmd opts =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack $ printf "(%s-%s) " cmd subcmd

getInputContent :: Input -> IO String
getInputContent (SubcommandInput name subname) = getHelpSub name subname
getInputContent (CommandInput name) = getHelp name
getInputContent (FileInput f) = readFile f

-- | Generate shell completion script from the root help page
-- as well as the subcommand's help pages.
toScriptFull :: Shell -> String -> IO Text
toScriptFull shell cmd = do
  rootContent <- getInputContent (CommandInput cmd)
  let rootOptions = parseMany rootContent
  let subcmds = parseSubcommand rootContent
  let subcmdsM = filterM (isSub cmd . _cmd) subcmds

  subs <- subcmdsM
  let subcmdNames = map _cmd subs
  let subcommandScript = genScriptSubcommands shell cmd subs
  let rootOptScript = genScriptRootOptions shell cmd subcmdNames rootOptions
  let two = [rootOptScript, subcommandScript]
  texts <- mapM (_writeSubcommandOptions shell cmd) subcmdNames
  let res =
        if null subcmdNames
          then trace "[warning] Ignore subcommands" $ genScriptSimple shell cmd rootOptions
          else T.intercalate "\n\n\n" (two ++ texts)
  return res

_writeSubcommandOptions :: Shell -> String -> String -> IO Text
_writeSubcommandOptions shell name subname = do
  content <- getInputContent (SubcommandInput name subname)
  let options = parseMany content
  let script = genScriptSubcommandOptions shell name subname options
  return script

_getSubcommandOpts :: String -> String -> IO [Opt]
_getSubcommandOpts name subname = parseMany <$> getInputContent (SubcommandInput name subname)

toCommandIO :: String -> IO Command
toCommandIO cmd = do
  rootContent <- getInputContent (CommandInput cmd)
  let rootOptions = parseMany rootContent
  let subcmds = nubSort (parseSubcommand rootContent)
  let subcmdsFilteredM = filterM (isSub cmd . _cmd) subcmds
  subs <- subcmdsFilteredM
  let subcmdNames = map _cmd subs
  let optsListM = mapM (_getSubcommandOpts cmd) subcmdNames
  let subcmdOptsPairsM = zip <$> subcmdsFilteredM <*> optsListM
  toCommand cmd cmd rootOptions <$> subcmdOptsPairsM

-- | Generate shell completion script from the root help page
-- as well as the subcommand's help pages.
writeJSON :: String -> IO ()
writeJSON = TIO.putStr . toJSONText <=< toCommandIO

listSubcommandsIO :: String -> IO [String]
listSubcommandsIO cmd = do
  rootContent <- getInputContent (CommandInput cmd)
  let subcmdNames = nubSort (map _cmd (parseSubcommand rootContent))
  filterM (isSub cmd) subcmdNames
