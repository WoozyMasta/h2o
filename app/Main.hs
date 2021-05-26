module Main where

import Control.Monad (filterM)
import Data.List.Extra (stripInfix)
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)
import GenBashCompletions (genBashScript)
import GenFishCompletions
  ( genFishScriptRootOptions,
    genFishScriptSimple,
    genFishScriptSubcommandOptions,
    genFishScriptSubcommands,
  )
import GenJSON (toCommand, writeCommandAsJSON)
import GenZshCompletions (genZshScript)
import HelpParser (Opt)
import Layout (parseMany, preprocessAll)
import Options.Applicative
import Subcommand (Subcommand (..), parseSubcommand)
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Utils (convertTabsToSpaces)

data Input
  = SubcommandInput String String
  | CommandInput String
  | FileInput FilePath

data Config = Config
  { _input :: Input,
    _shell :: String,
    _isOutputJSON :: Bool,
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
          <> help "Select shell for completion script (bash|zsh|fish|none)"
      )
    <*> switch
      ( long "json"
          <> help "Output in JSON"
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
run (Config (CommandInput name) _ True _ _ _) = trace "[main] JSON output\n" $ writeJSON name
run (Config input shell _ isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly) = do
  content <- getInputContent input
  let subcommands = parseSubcommand content
  let opts = parseMany content
  let res
        | isConvertingTabsToSpaces =
          trace "[main] Converting tags to spaces...\n" $
            putStr . convertTabsToSpaces 8 $ content
        | isPreprocessOnly =
          trace "[main] processing (option+arg, description) splitting only" $
            putStr . formatStringPairs . preprocessAll $ content
        | isListingSubcommands =
          trace "[main] Listing subcommands...\n" $ putStr (unlines (map _cmd subcommands))
        | otherwise =
          case input of
            SubcommandInput _ subname ->
              trace "[main] processing subcommand-level options" $
                putStr (genScriptSubcommandOptions shell cmd subname opts)
            CommandInput name ->
              writeWithSubcommands shell name
            FileInput _ ->
              trace "[main] processing options from the file" $
                putStr (genScriptSimple shell cmd opts)
  res
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

isSub :: String -> String -> IO Bool
isSub cmd subcmd = do
  content <- getHelpSub cmd subcmd
  contentRoot <- getHelp cmd
  return $ not (null content) && (content /= contentRoot)

genScriptSimple :: String -> String -> [Opt] -> String
genScriptSimple "fish" = genFishScriptSimple
genScriptSimple "zsh" = genZshScript
genScriptSimple "bash" = genBashScript
genScriptSimple _ = \_ opts -> unlines $ map show opts

genScriptRootOptions :: String -> String -> [String] -> [Opt] -> String
genScriptRootOptions "fish" cmd subcmds opts = genFishScriptRootOptions cmd subcmds opts
genScriptRootOptions shell cmd _ opts = genScriptSimple shell cmd opts

genScriptSubcommands :: String -> String -> [Subcommand] -> String
genScriptSubcommands "fish" cmd subcmds = genFishScriptSubcommands cmd subcmds
genScriptSubcommands _ _ subcmds = unlines $ map show subcmds

genScriptSubcommandOptions :: String -> String -> String -> [Opt] -> String
genScriptSubcommandOptions "fish" cmd subcmd opts = genFishScriptSubcommandOptions cmd subcmd opts
genScriptSubcommandOptions _ cmd subcmd opts =
  unlines $ map (\opt -> prefix ++ show opt) opts
  where
    prefix = printf "(%s-%s) " cmd subcmd

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
  let rootOptions = parseMany rootContent
  let subcmds = parseSubcommand rootContent
  let subcmdsM = filterM (isSub cmd . _cmd) subcmds

  subs <- subcmdsM
  let subcmdNames = map _cmd subs
  let subcommandScript = genScriptSubcommands shell cmd subs
  let rootOptScript = genScriptRootOptions shell cmd subcmdNames rootOptions

  if null subs
    then
      trace "[warning] Ignore subcommands" $
        putStr (genScriptSimple shell cmd rootOptions)
    else putStr (rootOptScript ++ "\n\n\n" ++ subcommandScript ++ "\n\n\n") >> mapM_ (_writeSubcommandOptions shell cmd) subcmdNames

_writeSubcommandOptions :: String -> String -> String -> IO ()
_writeSubcommandOptions shell name subname = do
  content <- getInputContent (SubcommandInput name subname)
  let options = parseMany content
  let script = genScriptSubcommandOptions shell name subname options
  putStr script
  putStr $ if null script then "" else "\n\n\n"

_getSubcommandOpts :: String -> String -> IO [Opt]
_getSubcommandOpts name subname = parseMany <$> getInputContent (SubcommandInput name subname)

-- | Generate shell completion script from the root help page
-- as well as the subcommand's help pages.
writeJSON :: String -> IO ()
writeJSON cmd = do
  rootContent <- getInputContent (CommandInput cmd)
  let rootOptions = parseMany rootContent
  let subcmds = parseSubcommand rootContent
  let subcmdsFilteredM = filterM (isSub cmd . _cmd) subcmds

  subs <- subcmdsFilteredM
  let subcmdNames = map _cmd subs
  let optsListM = mapM (_getSubcommandOpts cmd) subcmdNames
  let subcmdOptsPairsM = zip <$> subcmdsFilteredM <*> optsListM
  let commandM = toCommand cmd cmd rootOptions <$> subcmdOptsPairsM
  writeCommandAsJSON =<< commandM
