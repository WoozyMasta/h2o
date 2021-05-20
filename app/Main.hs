module Main where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import GenBashCompletions (genBashScript)
import GenFishCompletions
  ( genFishLineSubcommand,
    genFishScript,
    genFishScriptUnderSubcommand,
  )
import GenZshCompletions (genZshScript)
import HelpParser (Opt)
import Layout (getOptionDescriptionPairsFromLayout, parseMany, preprocessAll)
import Options.Applicative
import Subcommand (Subcommand, parseSubcommand)
import Utils (convertTabsToSpaces)

data Config = Config
  { input :: String,
    shell :: String,
    name :: String,
    subname :: String,
    isParsingSubcommand :: Bool,
    isConvertingTabsToSpaces :: Bool,
    isPreprocessOnly :: Bool
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
          <> showDefault
          <> value "mycli"
          <> help "Specify command name"
      )
    <*> strOption
      ( long "subname"
          <> metavar "<SUBCOMMAND>"
          <> showDefault
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
main = run =<< execParser opts
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "Parse manpage/help and generate shell completion script"
            <> header "shellcomp"
        )

run :: Config -> IO ()
run (Config f shell name subname isParsing isConvertingTabsToSpaces isPreprocessOnly) = do
  content <- readFile f
  let subcommands = parseSubcommand content
  let opts = parseMany content
  let s
        | isConvertingTabsToSpaces = convertTabsToSpaces 8 content
        | isPreprocessOnly = formatStringPairs $ preprocessAll content
        | isParsing = genSubcommandScript name subcommands
        | null subname = genOptScript shell name opts
        | otherwise = genSubcommandOptScript name subname opts
  putStr s
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])

genOptScript :: String -> String -> [Opt] -> String
genOptScript "fish" = genFishScript
genOptScript "zsh" = genZshScript
genOptScript "bash" = genBashScript
genOptScript _ = \_ opts -> unlines $ map show opts

genSubcommandScript :: String -> [Subcommand] -> String
genSubcommandScript cmd subcmds = unlines [genFishLineSubcommand cmd sub | sub <- subcmds]

genSubcommandOptScript :: String -> String -> [Opt] -> String
genSubcommandOptScript = genFishScriptUnderSubcommand
