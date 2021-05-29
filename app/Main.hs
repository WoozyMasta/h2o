{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (guard, (<=<))
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
import System.Process (readProcess)
import Text.Printf (printf)
import Type (Command (..), Opt, Subcommand (..))
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
          <> metavar "<string-string>"
          <> help "Specify command-subcommand pair, like git-log, to parse specific subcommand."
      )

commandInput :: Parser Input
commandInput =
  CommandInput
    <$> strOption
      ( long "command"
          <> short 'c'
          <> metavar "<string>"
          <> help "Specify command to parse. Subcommand pages are scanned automatically."
      )

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "<file>"
          <> help "Select a text file to parse."
      )

inputP :: Parser Input
inputP = commandInput <|> fileInput <|> subcommandInput

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

toScriptFull :: Shell -> String -> IO Text
toScriptFull shell cmd = do
  (Command _ _ rootOptions subs) <- toCommandIO cmd
  let subcmdNames = map _name subs
  let subcmdOptsPairs = [(_name sub, _options sub) | sub <- subs]
  let rootOptScript = genScriptRootOptions shell cmd subcmdNames rootOptions
  let subcommandScript = genScriptSubcommands shell cmd (toSubcommands subs)
  let two = [rootOptScript, subcommandScript]
  let texts = map (uncurry (genScriptSubcommandOptions shell cmd)) subcmdOptsPairs
  let res =
        if null subcmdNames
          then trace "[warning] Ignore subcommands" $ genScriptSimple shell cmd rootOptions
          else T.intercalate "\n\n\n" (two ++ texts)
  return res
  where
    toSubcommands :: [Command] -> [Subcommand]
    toSubcommands xs = [Subcommand name desc | (Command name desc _ _) <- xs]

toCommandIO :: String -> IO Command
toCommandIO cmd = do
  rootContent <- getInputContent (CommandInput cmd)
  let rootOptions = parseMany rootContent
  let subcmdCandidates = nubSort (parseSubcommand rootContent)
  let toSubcmdOptPair sub = do
        page <- getHelpSub cmd (_cmd sub)
        guard (not (null page) && page /= rootContent)
        return (sub, parseMany page)
  let subcmdOptsPairsM = mapM toSubcmdOptPair subcmdCandidates
  toCommand cmd cmd rootOptions <$> subcmdOptsPairsM

-- | Generate shell completion script from the root help page
-- as well as the subcommand's help pages.
writeJSON :: String -> IO ()
writeJSON = TIO.putStr . toJSONText <=< toCommandIO

listSubcommandsIO :: String -> IO [String]
listSubcommandsIO s = getSubnames <$> toCommandIO s

getSubnames :: Command -> [String]
getSubnames = map _name . _subcommands
