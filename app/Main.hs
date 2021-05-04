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
    subcommand :: Bool
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
          <> short 's'
          <> metavar "<SHELL>"
          <> showDefault
          <> value "none"
          <> help "Select shell for a completions script (bash/zsh/fish/none)"
      )
    <*> switch
      ( long "subcommand"
          <> short 'c'
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
run (Config f shell _) = do
  content <- readFile f
  let opts = parseMany content
  let cmd = "nanachi"
  putStr $ gen shell cmd opts

gen :: String -> String -> [Opt] -> String
gen "fish" = genFishScript
gen "zsh" = genZshScript
gen "bash" = genBashScript
gen _ = \cmd opts -> List.intercalate "\n" $ map show opts
