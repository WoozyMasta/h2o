module Main where

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
import Subcommand (Subcommand, parseSubcommand)
import System.FilePath (takeBaseName)
import Utils (convertTabsToSpaces)

data Config = Config
  { _input :: String,
    _shell :: String,
    _name :: String,
    _subname :: String,
    _isParsingSubcommand :: Bool,
    _isConvertingTabsToSpaces :: Bool,
    _isPreprocessOnly :: Bool
  }

config :: Parser Config
config =
  Config
    <$> strArgument
      ( metavar "<FILE>"
          <> help "Select a text file to parse."
      )
    <*> strOption
      ( long "shell"
          <> metavar "<SHELL>"
          <> showDefault
          <> value "none"
          <> help "Select shell for a completions script (bash/zsh/fish/none)"
      )
    <*> strOption
      ( long "name"
          <> short 'n'
          <> metavar "<COMMAND>"
          <> value ""
          <> help "Specify command name"
      )
    <*> strOption
      ( long "subname"
          <> metavar "<SUBCOMMAND>"
          <> value ""
          <> help "Specify subcommand name"
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
run (Config f shell name subname isParsing isConvertingTabsToSpaces isPreprocessOnly) = do
  content <- readFile f
  let subcommands = parseSubcommand content
  let opts = parseMany content
  let s
        | isConvertingTabsToSpaces = convertTabsToSpaces 8 content
        | isPreprocessOnly = formatStringPairs $ preprocessAll content
        | isParsing = genSubcommandScript cmd subcommands
        | null subname = genOptScript shell cmd opts
        | otherwise = genSubcommandOptScript cmd subname opts
  putStr s
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    cmd = if null name then takeBaseName f else name

genOptScript :: String -> String -> [Opt] -> String
genOptScript "fish" = genFishScript
genOptScript "zsh" = genZshScript
genOptScript "bash" = genBashScript
genOptScript _ = \_ opts -> unlines $ map show opts

genSubcommandScript :: String -> [Subcommand] -> String
genSubcommandScript cmd subcmds = unlines [genFishLineSubcommand cmd sub | sub <- subcmds]

genSubcommandOptScript :: String -> String -> [Opt] -> String
genSubcommandOptScript = genFishScriptUnderSubcommand
