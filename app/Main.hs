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
    parseSubcommand :: Bool
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
    <*> switch
      ( long "with-subcommand"
          <> help "Enable subcommand parsing"
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
run (Config f shell name _) = do
  content <- readFile f
  let opts = parseMany content
  putStr $ gen shell name opts

gen :: String -> String -> [Opt] -> String
gen "fish" = genFishScript
gen "zsh" = genZshScript
gen "bash" = genBashScript
gen _ = \_ opts -> unlines $ map show opts
