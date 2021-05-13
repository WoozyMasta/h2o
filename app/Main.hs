module Main where

import qualified Data.List as List
import Data.Semigroup ((<>))
import GenBashCompletions
import GenFishCompletions
import GenZshCompletions
import HelpParser
import Options.Applicative
import Subcommand

data Config = Config
  { input :: String,
    shell :: String,
    name :: String,
    subname :: String,
    isParsingSubcommand :: Bool,
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
run (Config f shell name subname isParsing isPreprocessOnly) = do
  content <- readFile f
  let subcommands = parseSubcommand content
  let opts = parseMany content
  let s
        | isPreprocessOnly = unlines . map show $ preprocessAll content
        | isParsing = genSubcommandScript name subcommands
        | null subname = genOptScript shell name opts
        | otherwise = genSubcommandOptScript name subname opts
  putStr s

genOptScript :: String -> String -> [Opt] -> String
genOptScript "fish" = genFishScript
genOptScript "zsh" = genZshScript
genOptScript "bash" = genBashScript
genOptScript _ = \_ opts -> unlines $ map show opts

genSubcommandScript :: String -> [Subcommand] -> String
genSubcommandScript cmd subcmds = unlines [genFishLineSubcommand cmd sub | sub <- subcmds]

genSubcommandOptScript :: String -> String -> [Opt] -> String
genSubcommandOptScript = genFishScriptUnderSubcommand
