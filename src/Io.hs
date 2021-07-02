{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Io where

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..))
import qualified Constants
import Control.Concurrent.ParallelIO.Global (extraWorkerWhileBlocked, parallelFirst)
import Control.Exception (SomeException, try)
import qualified Data.Either as Either
import qualified Data.Map.Ordered as OMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GenBashCompletions (genBashScript, toBashScript)
import GenFishCompletions (genFishScriptSimple, toFishScript)
import GenJSON (toJSONText)
import GenZshCompletions (genZshScript, toZshScript)
import Layout (parseMany, preprocessAll)
import Subcommand (parseSubcommand)
import qualified System.Exit
import System.FilePath (takeBaseName)
import qualified System.Process as Process
import Text.Printf (printf)
import Type (Command (..), Opt, Subcommand (..), asSubcommand, toCommand)
import Utils (convertTabsToSpaces, infoMsg, infoTrace, mayContainsOptions, mayContainsSubcommands, warnTrace)

run :: ConfigOrVersion -> IO Text
run Version = return (T.concat ["h2o ", Constants.versionStr, "\n"])
run (C_ (Config input _ isExportingJSON isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly isShallowOnly))
  | isExportingJSON = infoTrace "io: JSON output" $ case input of
    (CommandInput n) -> toJSONText <$> toCommandIO n
    (SubcommandInput n subn) -> toJSONSimple (n ++ "-" ++ subn) <$> (getInputContent input =<< isBwrapAvailableIO)
    (FileInput _) ->
      if isShallowOnly
        then toJSONSimple name <$> getInputContent input False
        else toJSONText <$> (pageToCommandIO name =<< getInputContent input False)
  | isConvertingTabsToSpaces = infoTrace "io: Converting tags to spaces...\n" $ T.pack . convertTabsToSpaces 8 <$> (getInputContent input =<< isBwrapAvailableIO)
  | isListingSubcommands = infoTrace "io: Listing subcommands...\n" $ T.unlines . map T.pack <$> listSubcommandsIO name
  | isPreprocessOnly = infoTrace "io: processing (option+arg, description) splitting only" $ T.pack . formatStringPairs . preprocessAll <$> (getInputContent input =<< isBwrapAvailableIO)
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    name = case input of
      CommandInput n -> n
      SubcommandInput n _ -> n
      FileInput f -> takeBaseName f
run (C_ (Config (CommandInput name) outputFormat _ _ _ _ _)) = toScriptFull outputFormat <$> toCommandIO name
run (C_ (Config (SubcommandInput name subname) outputFormat _ _ _ _ _)) =
  infoTrace (printf "io: processing subcommand-level options (%s, %s)" name subname) toScriptSimple outputFormat cmdSubcmdName <$> optsIO
  where
    cmdSubcmdName = name ++ "-" ++ subname
    optsIO = parseMany <$> (getInputContent (SubcommandInput name subname) =<< isBwrapAvailableIO)
run (C_ (Config (FileInput f) outputFormat _ _ _ _ isShallowOnly))
  | isShallowOnly = infoTrace "io: processing just the file" $ toScriptSimple outputFormat name <$> optsIO
  | otherwise = infoTrace "io: processing the file and more" $ toScriptFull outputFormat <$> (pageToCommandIO name =<< contentIO)
  where
    name = takeBaseName f
    optsIO = parseMany <$> getInputContent (FileInput f) False
    contentIO = getInputContent (FileInput f) False

getHelp :: Bool -> String -> IO String
getHelp True = infoTrace ("sandboxed" :: String) getHelpSandboxed
getHelp False = warnTrace ("No sandboxing!" :: String) getHelpBare

getHelpSub :: Bool -> String -> String -> IO String
getHelpSub True = getHelpSubSandboxed
getHelpSub False = getHelpSubBare

-- | Following implementation takes more than 2x longer than the above... WHY??
-- |
getHelpTemplate :: String -> [[String]] -> IO String
getHelpTemplate _ [] = return ""
getHelpTemplate name (args : argsBag) = do
  mx <- try (fetchHelpInfo name args) :: IO (Either SomeException (Maybe String))
  if Either.isLeft mx
    then return ""
    else
      maybe
        (fmap (Maybe.fromMaybe "") $ parallelFirst $ map (fetchHelpInfo name) argsBag)
        return
        (Either.fromRight Nothing mx)

fetchHelpInfo :: FilePath -> [String] -> IO (Maybe String)
fetchHelpInfo name args = do
  (exitCode, stdout, stderr) <- Process.readProcessWithExitCode name args ""
  let res
        | isCommandNotFound name exitCode stderr = error "CommandNotFound"
        | not (null stdout) = Just stdout
        | mayContainsOptions stderr || mayContainsSubcommands stderr = Just stderr
        | otherwise = Nothing
  return res

isCommandNotFound :: String -> System.Exit.ExitCode -> String -> Bool
isCommandNotFound name exitCode stderr =
  exitCode == System.Exit.ExitFailure 127
    || ("bwrap: execvp " `T.append` T.pack name) `T.isPrefixOf` T.pack stderr

getHelpBare :: String -> IO String
getHelpBare name = getHelpTemplate name [["--help"], ["help"]]

getHelpSubBare :: String -> String -> IO String
getHelpSubBare name subname = getHelpTemplate name [[subname, "--help"], ["help", subname]]

bwrapArgsBase :: [String]
bwrapArgsBase = ["--ro-bind", "/", "/", "--dev", "/dev", "--tmpfs", "/tmp", "--unshare-all"]

getHelpSandboxed :: String -> IO String
getHelpSandboxed name = getHelpTemplate "bwrap" [options, altOptions]
  where
    options = bwrapArgsBase ++ [name, "--help"]
    altOptions = bwrapArgsBase ++ [name, "help"]

getHelpSubSandboxed :: String -> String -> IO String
getHelpSubSandboxed name subname = getHelpTemplate "bwrap" [options, altOptions]
  where
    options = bwrapArgsBase ++ [name, subname, "--help"]
    altOptions = bwrapArgsBase ++ [name, "help", subname]

getMan :: String -> IO String
getMan name = do
  (exitCode, stdout, _) <- Process.readCreateProcessWithExitCode cp ""
  if exitCode == System.Exit.ExitFailure 16
    then return ""
    else return stdout
  where
    s = printf "man %s | col -b" name
    cp = Process.shell s

getHelpAndMan :: Bool -> String -> IO String
getHelpAndMan isSandboxing name = do
  content <- getHelp isSandboxing name
  if null content
    then do
      content2 <- getMan name
      if null content2
        then error ("io: Neither help or man pages available: " ++ name)
        else infoTrace "io: Using manpage" $ return content2
    else infoTrace "io: Using help" $ return content

toScriptSimple :: OutputFormat -> String -> [Opt] -> Text
toScriptSimple Fish name opts = genFishScriptSimple name opts
toScriptSimple Zsh name opts = genZshScript name opts
toScriptSimple Bash name opts = genBashScript name opts
toScriptSimple _ _ opts = T.unlines $ map (T.pack . show) opts

toScriptRootOptions :: OutputFormat -> String -> [String] -> [Opt] -> Text
toScriptRootOptions outputFormat name _ = toScriptSimple outputFormat name

toScriptSubcommands :: OutputFormat -> String -> [Command] -> Text
toScriptSubcommands _ _ subcmds = T.unlines $ map (T.pack . show . asSubcommand) subcmds

toScriptSubcommandOptions :: OutputFormat -> String -> Command -> Text
toScriptSubcommandOptions _ name (Command subname _ opts _) =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack $ printf "(%s-%s) " name subname

getInputContent :: Input -> Bool -> IO String
getInputContent (SubcommandInput name subname) isSandboxing = getHelpSub isSandboxing name subname
getInputContent (CommandInput name) isSandboxing = getHelpAndMan isSandboxing name
getInputContent (FileInput f) _ = readFile f

toScriptFull :: OutputFormat -> Command -> Text
toScriptFull Fish cmd = toFishScript cmd
toScriptFull Zsh cmd = toZshScript cmd
toScriptFull Bash cmd = toBashScript cmd
toScriptFull outputFormat (Command name _ rootOptions subs)
  | null subnames = warnTrace "Ignore subcommands" $ toScriptSimple outputFormat name rootOptions
  | otherwise = T.intercalate "\n\n\n" (filter (not . T.null) entries)
  where
    subnames = map _name subs
    rootOptScript = toScriptRootOptions outputFormat name subnames rootOptions
    subcommandScript = toScriptSubcommands outputFormat name subs
    subcommandOptionScripts = [toScriptSubcommandOptions outputFormat name subcmd | subcmd <- subs]
    entries = [rootOptScript, subcommandScript] ++ subcommandOptionScripts

isBwrapAvailableIO :: IO Bool
isBwrapAvailableIO = (\(e, _, _) -> e == System.Exit.ExitSuccess) <$> Process.readProcessWithExitCode "bash" ["-c", "command -v bwrap"] ""

toCommandIO :: String -> IO Command
toCommandIO name = do
  !isSandboxing <- isBwrapAvailableIO
  content <- getInputContent (CommandInput name) isSandboxing
  toCommandIOHelper name content isSandboxing

pageToCommandIO :: String -> String -> IO Command
pageToCommandIO name content = do
  !isSandboxing <- isBwrapAvailableIO
  toCommandIOHelper name content isSandboxing

toCommandIOHelper :: String -> String -> Bool -> IO Command
toCommandIOHelper name content isSandboxing = do
  subcmdOptsPairs <- subcmdOptsPairsM
  if null rootOptions && null subcmdOptsPairs
    then error ("Failed to extract information for a Command: " ++ name)
    else return $ toCommand name name rootOptions subcmdOptsPairs
  where
    -- remove duplicate _cmd in [Subcommand]
    sub2pair (Subcommand s1 s2) = (s1, s2)
    pair2sub = uncurry Subcommand
    uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair
    rootOptions = parseMany content
    subcmdCandidates =
      infoMsg "subcommand candidates : \n" $ uniqSubcommands (parseSubcommand content)
    toSubcmdOptPair sub = do
      page <- getHelpSub isSandboxing name (_cmd sub)
      let criteria = not (null page) && page /= content
      return ((sub, parseMany page), criteria)
    subcmdOptsPairsM = map fst . filter snd <$> mapM toSubcmdOptPair subcmdCandidates

-- Convert to Command given command name and text
toCommandSimple :: String -> String -> Command
toCommandSimple name content =
  if null rootOptions
    then error ("Failed to extract information for a Command: " ++ name)
    else Command name name rootOptions []
  where
    rootOptions = parseMany content

toJSONSimple :: String -> String -> Text
toJSONSimple name content = toJSONText (toCommandSimple name content)

listSubcommandsIO :: String -> IO [String]
listSubcommandsIO name = getSubnames <$> toCommandIO name

getSubnames :: Command -> [String]
getSubnames = map _name . _subcommands
