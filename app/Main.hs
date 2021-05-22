module Main where

import Data.List.Extra (stripInfix)
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)
import GenBashCompletions (genBashScript)
import GenFishCompletions
  ( genFishLineSubcommand,
    genFishScript,
    genFishScriptUnderSubcommand,
  )
import GenZshCompletions (genZshScript)
import HelpParser (Opt)
import Layout (parseMany, preprocessAll)
import Options.Applicative
import Subcommand (Subcommand (..), parseSubcommand)
import System.FilePath (takeBaseName)
import System.Process (readProcess)
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
          <> metavar "<STRING>-<STRING>"
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
run (Config input shell isParsingSubcommand isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly) = do
  content <- case input of
    SubcommandInput name subname -> getHelpSub name subname
    CommandInput name -> getHelp name
    FileInput f -> readFile f
  let subcommands = parseSubcommand content
  let opts = parseMany content
  let s
        | isConvertingTabsToSpaces =
          trace "[main] Converting tags to spaces...\n" $
            convertTabsToSpaces 8 content
        | isListingSubcommands =
          trace "[main] Listing subcommands...\n" $ unlines (map _cmd subcommands)
        | isParsingSubcommand && isPreprocessOnly =
          trace "[main] processing subcommands only" $
            genSubcommandScript cmd subcommands
        | isPreprocessOnly =
          trace "[main] processing (option+arg, description) splitting only" $
            formatStringPairs $ preprocessAll content
        | otherwise =
          case input of
            SubcommandInput _ subname ->
              if isParsingSubcommand
                then
                  trace "[main] processing subcommands + subcommand-level options" $
                    genSubcommandScript cmd subcommands ++ "\n\n\n" ++ genSubcommandOptScript cmd subname opts
                else
                  trace "[main] processing subcommand-level options" $
                    genSubcommandOptScript cmd subname opts
            _ ->
              trace "[main] processing subcommands + top-level options" $
                genSubcommandScript cmd subcommands ++ "\n\n\n" ++ genOptScript shell cmd opts
  putStr s
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    cmd = case input of
      SubcommandInput name _ -> name
      CommandInput name -> name
      FileInput f -> takeBaseName f

getHelp :: String -> IO String
getHelp cmd = readProcess cmd ["--help"] ""

getHelpSub :: String -> String -> IO String
getHelpSub cmd subcmd = readProcess cmd [subcmd, "--help"] ""

genOptScript :: String -> String -> [Opt] -> String
genOptScript "fish" = genFishScript
genOptScript "zsh" = genZshScript
genOptScript "bash" = genBashScript
genOptScript _ = \_ opts -> unlines $ map show opts

genSubcommandScript :: String -> [Subcommand] -> String
genSubcommandScript cmd subcmds = unlines [genFishLineSubcommand cmd sub | sub <- subcmds]

genSubcommandOptScript :: String -> String -> [Opt] -> String
genSubcommandOptScript = genFishScriptUnderSubcommand
