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
