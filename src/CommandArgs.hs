{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CommandArgs where

import Data.List.Extra (stripInfix)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Options.Applicative

data Input
  = CommandInput String Bool
  | FileInput FilePath Bool
  | SubcommandInput String String Bool
  | JsonInput FilePath

data Config = Config
  { _input :: Input,
    _shell :: OutputFormat,
    _isOutputJSON :: Bool,
    _isConvertingTabsToSpaces :: Bool,
    _isListingSubcommands :: Bool,
    _isPreprocessOnly :: Bool,
    _isShallowOnly :: Bool
  }

data ConfigOrVersion = Version | C_ Config

data OutputFormat = Bash | Zsh | Fish | Json | Native deriving (Eq, Show)

toOutputFormat :: String -> OutputFormat
toOutputFormat s
  | s' == "bash" = Bash
  | s' == "zsh" = Zsh
  | s' == "fish" = Fish
  | s' == "json" = Json
  | otherwise = Native
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
    <*> switch
      ( long "skip-man"
          <> help "Skip scanning manpage and focus on help text. Does not apply if input source is a file."
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
    <*> switch
      ( long "skip-man"
          <> help "Skip scanning manpage and focus on help text. Does not apply if input source is a file."
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
    <*> switch
      ( long "skip-man"
          <> help "Skip scanning manpage and focus on help text. Does not apply if input source is a file."
      )


jsonInput :: Parser Input
jsonInput =
  JsonInput
    <$> strOption
      ( long "loadjson"
          <> metavar "<file>"
          <> help "Load JSON file in Command schema."
      )

inputP :: Parser Input
inputP = commandInput <|> fileInput <|> subcommandInput <|> jsonInput

config :: Parser ConfigOrVersion
config =
  C_
    <$> ( Config
            <$> inputP
            <*> ( toOutputFormat
                    <$> strOption
                      ( long "format"
                          <> metavar "{bash|zsh|fish|json|native}"
                          <> showDefault
                          <> value "native"
                          <> help "Select output format of the completion script (bash|zsh|fish|json|native)"
                      )
                )
            <*> switch
              ( long "json"
                  <> help "[Deprecated] Show parsed results in JSON. Use --format json instead."
              )
            <*> switch
              ( long "convert-tabs-to-spaces"
                  <> help "[Test only] Convert tabs to spaces"
              )
            <*> switch
              ( long "list-subcommands"
                  <> help "List subcommands"
              )
            <*> switch
              ( long "debug"
                  <> help "[Test only] Run preprocessing only (for debugging)"
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
