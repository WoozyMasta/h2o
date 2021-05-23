module Main where

import Data.List.Extra (stripInfix)
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)
import GenBashCompletions (genBashScript)
import GenFishCompletions
  ( genFishScriptSimple,
    genFishScriptRootOptions,
    genFishScriptSubcommands,
    genFishScriptSubcommandOptions,
  )
import GenZshCompletions (genZshScript)
import HelpParser (Opt)
import Layout (parseMany, preprocessAll)
import Options.Applicative
import Subcommand (Subcommand (..), parseSubcommand)
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)
import Utils (convertTabsToSpaces)

data Input
  = SubcommandInput String String
  | CommandInput String
  | FileInput FilePath

data Config = Config
  { _input :: Input,
    _shell :: String,
    _isParsingSubcommand :: Bool,
    _isConvertingTabsToSpaces :: Bool,
    _isListingSubcommands :: Bool,
    _isPreprocessOnly :: Bool
  }

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
    <*> strOption
      ( long "shell"
          <> metavar "{bash|zsh|fish|none}"
          <> showDefault
          <> value "none"
          <> help "Select shell for a completions script (bash|zsh|fish|none)"
      )
    <*> switch
      ( long "parse-subcommand"
          <> help "Parse subcommands (experimental)"
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
run (Config input shell isParsingSubcommand isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly)
  | isParsingSubcommand = case input of
    CommandInput name -> writeWithSubcommands shell name
    _ -> putStr "[Usage] To parse all subcommand help pages, run `h2o --command <string> --parse-subcommand`"
  | otherwise = do
    content <- getInputContent input
    let subcommands = parseSubcommand content
    let opts = parseMany content
    let s
          | isConvertingTabsToSpaces =
            trace "[main] Converting tags to spaces...\n" $
              convertTabsToSpaces 8 content
          | isListingSubcommands =
            trace "[main] Listing subcommands...\n" $ unlines (map _cmd subcommands)
          | isPreprocessOnly =
            trace "[main] processing (option+arg, description) splitting only" $
              formatStringPairs $ preprocessAll content
          | otherwise =
            case input of
              SubcommandInput _ subname ->
                trace "[main] processing subcommand-level options" $
                  genScriptSubcommandOptions cmd subname opts
              _ ->
                trace "[main] processing top-level options" $
                  genScriptSimple shell cmd opts
    putStr s
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    cmd = case input of
      SubcommandInput name _ -> name
      CommandInput name -> name
      FileInput f -> takeBaseName f

getHelp :: String -> IO String
getHelp cmd = do
  (_, content, _) <- readProcessWithExitCode cmd ["--help"] ""
  if null content
    then (\(_, s, _) -> s) <$> readProcessWithExitCode cmd ["help"] ""
    else return content

getHelpSub :: String -> String -> IO String
getHelpSub cmd subcmd = do
  (_, content, _) <- readProcessWithExitCode cmd [subcmd, "--help"] ""
  if null content
    then (\(_, s, _) -> s) <$> readProcessWithExitCode cmd ["help", subcmd] "" -- samtools
    else return content

genScriptSimple :: String -> String -> [Opt] -> String
genScriptSimple "fish" = genFishScriptSimple
genScriptSimple "zsh" = genZshScript
genScriptSimple "bash" = genBashScript
genScriptSimple _ = \_ opts -> unlines $ map show opts

genScriptRootOptions :: String -> [String] -> [Opt] -> String
genScriptRootOptions = genFishScriptRootOptions

genScriptSubcommands :: String -> [Subcommand] -> String
genScriptSubcommands = genFishScriptSubcommands

genScriptSubcommandOptions :: String -> String -> [Opt] -> String
genScriptSubcommandOptions = genFishScriptSubcommandOptions

getInputContent :: Input -> IO String
getInputContent input = case input of
  SubcommandInput name subname -> getHelpSub name subname
  CommandInput name -> getHelp name
  FileInput f -> readFile f

-- | Generate shell completion script from the root help page
-- as well as the subcommand's help pages.
writeWithSubcommands :: String -> String -> IO ()
writeWithSubcommands shell cmd = do
  rootContent <- getInputContent (CommandInput cmd)
  let subcmds = parseSubcommand rootContent
  let subcommandScript = genScriptSubcommands cmd subcmds
  putStr subcommandScript
  putStr $ if null subcommandScript then "" else "\n\n\n"

  let rootOptions = parseMany rootContent
  let subcmdNames = map _cmd subcmds
  let rootOptScript = genScriptRootOptions cmd subcmdNames rootOptions
  putStr rootOptScript
  putStr $ if null rootOptScript then "" else "\n\n\n"

  let subcommandList = map _cmd subcmds
  mapM_ (_writeSubcommandOptions shell cmd) subcommandList

_writeSubcommandOptions :: String -> String -> String -> IO ()
_writeSubcommandOptions _ name subname = do
  content <- getInputContent (SubcommandInput name subname)
  let options = parseMany content
  let script = genScriptSubcommandOptions name subname options
  putStr script
  putStr $ if null script then "" else "\n\n\n"
