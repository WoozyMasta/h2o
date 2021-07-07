{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Io where

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..))
import qualified Constants
import Control.Concurrent.ParallelIO.Global (parallelFirst)
import Control.Exception (SomeException, try)
import qualified Data.Either as Either
import qualified Data.Map.Ordered as OMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GenBashCompletions (genBashScript, toBashScript)
import GenFishCompletions (genFishScriptSimple, toFishScript)
import GenJSON (toJSONText)
import GenZshCompletions (genZshScript, toZshScript)
import Layout (parseMany, preprocessAll)
import Subcommand (parseSubcommand)
import qualified System.Exit
import System.FilePath (takeBaseName)
import qualified System.Process.Typed as Process
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
  | isConvertingTabsToSpaces = infoTrace "io: Converting tags to spaces...\n" $ T.pack <$> (getInputContent input =<< isBwrapAvailableIO)
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

getHelp :: Bool -> String -> IO Text
getHelp True = infoTrace ("sandboxed" :: String) getHelpSandboxed
getHelp False = warnTrace ("No sandboxing!" :: String) getHelpBare

getHelpSub :: Bool -> String -> String -> IO Text
getHelpSub True = getHelpSubSandboxed
getHelpSub False = getHelpSubBare

getManSub :: String -> String -> IO Text
getManSub name subname = getMan (name ++ "-" ++ subname)

getManAndHelpSub :: Bool -> String -> String -> IO Text
getManAndHelpSub isSandboxing name subname = do
  content <- getManSub name subname
  if T.null content
    then do
      content2 <- getHelpSub isSandboxing name subname
      if T.null content2
        then error ("io: Neither help or man pages available: " ++ name ++ "-" ++ subname)
        else infoTrace "io: Using help for subcommand" $ return content2
    else infoTrace "io: Using manpage for subcommand" $ return content

-- | Following implementation takes more than 2x longer than the above... WHY??
-- |
getHelpTemplate :: String -> [[String]] -> IO Text
getHelpTemplate _ [] = return ""
getHelpTemplate name (args : argsBag) = do
  mx <- try (fetchHelpInfo name args) :: IO (Either SomeException (Maybe Text))
  if Either.isLeft mx
    then return ""
    else
      maybe
        (fmap (Maybe.fromMaybe "") $ parallelFirst $ map (fetchHelpInfo name) argsBag)
        return
        (Either.fromRight Nothing mx)

fetchHelpInfo :: FilePath -> [String] -> IO (Maybe Text)
fetchHelpInfo name args = do
  (exitCode, stdout, stderr) <- Process.readProcess (Process.proc name args)
  let stdoutText = TL.toStrict . TLE.decodeUtf8 $ stdout
  let stderrText = TL.toStrict . TLE.decodeUtf8 $ stderr
  let res
        | isCommandNotFound name exitCode stderrText = error "CommandNotFound"
        | not (T.null stdoutText) = Just stdoutText
        | mayContainsOptions stderrText || mayContainsSubcommands stderrText = Just stderrText
        | otherwise = Nothing
  return res

isCommandNotFound :: String -> System.Exit.ExitCode -> Text -> Bool
isCommandNotFound name exitCode stderr =
  exitCode == System.Exit.ExitFailure 127
    || ("bwrap: execvp " `T.append` T.pack name) `T.isPrefixOf` stderr

getHelpBare :: String -> IO Text
getHelpBare name = getHelpTemplate name [["--help"], ["help"]]

getHelpSubBare :: String -> String -> IO Text
getHelpSubBare name subname = getHelpTemplate name [[subname, "--help"], ["help", subname]]

bwrapArgsBase :: [String]
bwrapArgsBase = ["--ro-bind", "/", "/", "--dev", "/dev", "--tmpfs", "/tmp", "--unshare-all"]

getHelpSandboxed :: String -> IO Text
getHelpSandboxed name = getHelpTemplate "bwrap" [options, altOptions]
  where
    options = bwrapArgsBase ++ [name, "--help"]
    altOptions = bwrapArgsBase ++ [name, "help"]

getHelpSubSandboxed :: String -> String -> IO Text
getHelpSubSandboxed name subname = getHelpTemplate "bwrap" [options, altOptions]
  where
    options = bwrapArgsBase ++ [name, subname, "--help"]
    altOptions = bwrapArgsBase ++ [name, "help", subname]

getMan :: String -> IO Text
getMan name = do
  (exitCode, stdout, _) <- Process.readProcess cp
    -- The exit code is actually thrown when piped to others...
  if exitCode == System.Exit.ExitFailure 16
    then return ""
    else return . TL.toStrict . TLE.decodeUtf8 $ stdout
  where
    cp = Process.shell $ printf "man %s | col -bx" name

getManAndHelp :: Bool -> String -> IO Text
getManAndHelp isSandboxing name = do
  content <- getMan name
  if T.null content
    then do
      content2 <- getHelp isSandboxing name
      if T.null content2
        then error ("io: Neither help or man pages available: " ++ name)
        else infoTrace "io: Using help" $ return content2
    else infoTrace "io: Using manpage" $ return content

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
getInputContent (SubcommandInput name subname) isSandboxing =
  T.unpack . Utils.convertTabsToSpaces 8 <$> getManAndHelpSub isSandboxing name subname
getInputContent (CommandInput name) isSandboxing =
  T.unpack . Utils.convertTabsToSpaces 8 <$> getManAndHelp isSandboxing name
getInputContent (FileInput f) _ =
  T.unpack . Utils.convertTabsToSpaces 8 . T.pack <$> readFile f

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
isBwrapAvailableIO = (\(e, _, _) -> e == System.Exit.ExitSuccess) <$> Process.readProcess (Process.proc "bash" ["-c", "command -v bwrap"])

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
    sub2pair (Subcommand s1 s2) = (s1, s2)
    pair2sub = uncurry Subcommand
    uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair
    rootOptions = parseMany content
    subcmdCandidates =
      infoMsg "subcommand candidates : \n" $ uniqSubcommands (parseSubcommand content)
    toSubcmdOptPair useMan sub = do
      page <- if useMan then getManSub name (_cmd sub) else getHelpSub isSandboxing name (_cmd sub)
      let criteria = not (T.null page) && page /= T.pack content
      return ((sub, parseMany (T.unpack page)), criteria)
    pairsIO = do
      !isManAvailable <- not . T.null <$> getMan name
      mapM (toSubcmdOptPair isManAvailable) subcmdCandidates
    subcmdOptsPairsM = map fst . filter snd <$> pairsIO

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
