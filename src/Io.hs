{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Io where

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..))
import qualified Constants
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Ordered as OMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GenBashCompletions (toBashScript)
import GenFishCompletions (toFishScript)
import GenJSON (toJSONText)
import GenZshCompletions (toZshScript)
import Layout (parseBlockwise, preprocessBlockwise)
import qualified Postprocess
import Subcommand (parseSubcommand)
import qualified System.Exit
import System.FilePath (takeBaseName)
import qualified System.Process.Typed as Process
import Text.Printf (printf)
import Type (Command (..), Opt, Subcommand (..), asSubcommand, toCommand)
import Utils (convertTabsToSpaces, infoMsg, infoTrace, mayContainUseful, warnTrace)
import qualified Utils
import qualified Data.List as List


-- | Main function processing ConfigOrVersion
run :: ConfigOrVersion -> IO Text
-- Just return version
run Version = return (T.concat ["h2o ", Constants.versionStr, "\n"])

-- Or, do some utility work
run (C_ (Config input _ isExportingJSON isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly isShallowOnly))
  | isExportingJSON = Utils.warnTrace "io: Deprecated: Use --format json instead" $ run (C_ (Config input Json False False False False isShallowOnly))
  | isConvertingTabsToSpaces = infoTrace "io: Converting tags to spaces...\n" T.pack <$> getInputContent input
  | isListingSubcommands = infoTrace "io: Listing subcommands...\n" $ T.unlines <$> listSubcommandsIO input
  | isPreprocessOnly = infoTrace "io: processing (option+arg, description) splitting only" $ T.pack . formatStringPairs . preprocessBlockwise <$> getInputContent input
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])

-- Or, process the input file in text
run (C_ (Config input@(FileInput f skipMan) format _ _ _ _ isShallowOnly))
  | isShallowOnly = infoTrace "io: processing just the file" $ toScript format . pageToCommandSimple name <$> contentIO
  | otherwise = infoTrace "io: processing the file and more" $ toScript format <$> (pageToCommandIO name skipMan =<< contentIO)
  where
    name = takeBaseName f
    contentIO = getInputContent input

-- Or, process with command name
run (C_ (Config input@(CommandInput name skipMan) format _ _ _ _ isShallowOnly))
  | isShallowOnly = toScript format . pageToCommandSimple name <$> contentIO
  | otherwise = toScript format <$> (pageToCommandIO name skipMan =<< contentIO)
  where
    contentIO = getInputContent input

-- Or, process with command name AND subcommand name
run (C_ (Config input@(SubcommandInput name subname _) format _ _ _ _ _)) =
  toScript format . pageToCommandSimple nameSubname <$> getInputContent input
  where
    nameSubname = name ++ "-" ++ subname

-- Or, load Command from JSON
run (C_ (Config input@(JsonInput _) format _ _ _ _ _)) = do
  content <- TLE.encodeUtf8 . TL.pack <$> getInputContent input
  let cmdMay = Aeson.decode content :: Maybe Command
  let commandIO =
        case cmdMay of
          Nothing -> error "Cannot decode JSON!"
          Just c -> return c
  toScript format <$> commandIO

getManSub :: [String] -> IO Text
getManSub names = getMan $ List.intercalate "-" names

getManAndHelpSub :: [String] -> IO Text
getManAndHelpSub names = do
  content <- getManSub names
  if T.null content
    then do
      content2 <- getHelpSub names
      if T.null content2
        then error ("io: Neither help or man pages available: " ++ List.intercalate "-" names)
        else infoTrace "io: Using help for subcommand" $ return content2
    else infoTrace "io: Using manpage for subcommand" $ return content

-- |
getHelpTemplate :: String -> [[String]] -> IO Text
getHelpTemplate _ [] = return ""
getHelpTemplate name (args : argsBag) = do
  emx <- try (fetchHelpInfo name args) :: IO (Either SomeException (Maybe Text))
  case emx of
    Left _ -> return ""
    Right mx -> case mx of
      Just x -> return x
      Nothing -> getHelpTemplate name argsBag

fetchHelpInfo :: String -> [String] -> IO (Maybe Text)
fetchHelpInfo name args = do
  (exitCode, stdout, stderr) <- Process.readProcess (Process.proc name args)
  let stdoutText = TL.toStrict . TLE.decodeUtf8 $ stdout
  let stderrText = TL.toStrict . TLE.decodeUtf8 $ stderr
  let res
        | isCommandNotFound name exitCode stderrText = error "CommandNotFound"
        | mayContainUseful stdoutText = Utils.debugTrace ("Using stdout: " ++ unwords (name : args)) $ Just stdoutText
        | mayContainUseful stderrText = Utils.debugTrace ("Using stderr: " ++ unwords (name : args)) $ Just stderrText
        | otherwise = Nothing
  return res

isCommandNotFound :: String -> System.Exit.ExitCode -> Text -> Bool
isCommandNotFound _ exitCode _ =
  exitCode == System.Exit.ExitFailure 127

getHelp :: String -> IO Text
getHelp name = getHelpTemplate name [["--help"], ["help"], ["-help"], ["-h"]]

getHelpSub :: [String] -> IO Text
getHelpSub names = getHelpTemplate name [[subname, "--help"], ["help", subname], [subname, "-help"], [subname, "-h"]]
  where
    name = unwords (init names)
    subname = last names

getMan :: String -> IO Text
getMan name = do
  (exitCode, stdout, _) <- Process.readProcess cp
  -- The exit code is actually thrown when piped to others...
  if exitCode == System.Exit.ExitFailure 16
    then return ""
    else return . TL.toStrict . TLE.decodeUtf8 $ stdout
  where
    cp = Process.shell $ printf "man %s | col -bx" name

getManAndHelp :: String -> IO Text
getManAndHelp name = do
  content <- getMan name
  if T.null content
    then do
      content2 <- getHelp name
      if T.null content2
        then error ("io: Neither help or man pages available: " ++ name)
        else infoTrace "io: Using help" $ return content2
    else infoTrace "io: Using manpage" $ return content

toScriptRootOptions :: [Opt] -> Text
toScriptRootOptions = T.unlines . map (T.pack . show)

toScriptSubcommands :: [Command] -> Text
toScriptSubcommands = T.unlines . map (T.pack . show . asSubcommand)

toScriptSubcommandOptions :: String -> Command -> Text
toScriptSubcommandOptions name (Command subname _ opts _) =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack $ printf "(%s-%s) " name subname

getInputContent :: Input -> IO String
getInputContent (SubcommandInput name subname skipMan) =
  T.unpack . Utils.dropUsage . Utils.convertTabsToSpaces 8 <$> reader [name, subname]
  where
    reader = if skipMan then getHelpSub else getManAndHelpSub

getInputContent (CommandInput name skipMan) =
  T.unpack . Utils.dropUsage . Utils.convertTabsToSpaces 8 <$> reader name
  where
    reader = if skipMan then getHelp else getManAndHelp
getInputContent (FileInput f _) =
  T.unpack . Utils.dropUsage . Utils.convertTabsToSpaces 8 . T.pack <$> readFile f
getInputContent (JsonInput f) = readFile f

toScript :: OutputFormat -> Command -> Text
toScript Fish cmd = toFishScript cmd
toScript Zsh cmd = toZshScript cmd
toScript Bash cmd = toBashScript cmd
toScript Json cmd = toJSONText cmd
toScript Native (Command name _ rootOptions subs)
  | null subs = warnTrace "Ignore subcommands" $ T.unlines $ map (T.pack . show) rootOptions
  | otherwise = T.intercalate "\n\n\n" (filter (not . T.null) entries)
  where
    rootOptScript = toScriptRootOptions rootOptions
    subcommandScript = toScriptSubcommands subs
    subcommandOptionScripts = [toScriptSubcommandOptions name subcmd | subcmd <- subs]
    entries = [rootOptScript, subcommandScript] ++ subcommandOptionScripts


-- | Scans over command and subcommands
-- `name` is the name of the command.
-- `skipMan` sets weather to read man pages in subsequent scans.
-- `content` is the top-level text to be scanned.
--
pageToCommandIO :: String -> Bool -> String -> IO Command
pageToCommandIO name skipMan content = do
  subcmdOptsPairs <- subcmdOptsPairsM
  if null rootOptions && null subcmdOptsPairs
    then error ("Failed to extract information for a Command: " ++ name)
    else return $ Postprocess.fixCommand $ toCommand name name rootOptions subcmdOptsPairs
  where
    -- get command options from `content`
    rootOptions = parseBlockwise content

    -- get subcommand candidates from `content`
    subcmdCandidates =
      infoMsg "subcommand candidates : \n" $ uniqSubcommands (parseSubcommand content)
      where
        sub2pair (Subcommand s1 s2) = (s1, s2)
        pair2sub = uncurry Subcommand
        uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair

    -- scan options specific to the subcommand `sub`
    toSubcmdOptPair useMan sub = do
      page <-
        Utils.dropUsage . Utils.convertTabsToSpaces 8 <$> readFunc [name, _cmd sub]
      let isSuccess = not (T.null page) && page /= T.pack content
      return ((sub, parseBlockwise (T.unpack page)), isSuccess)
      where
        readFunc = if useMan then getManSub else getHelpSub

    -- scan over subcommand candidates
    subcmdOptsPairsM = map fst . filter snd <$> pairsIO
      where
        pairsIO = do
          !isManAvailable <- isManAvailableIO name
          mapM (toSubcmdOptPair (not skipMan && isManAvailable)) subcmdCandidates


-- | Checks if man page is available
isManAvailableIO :: String -> IO Bool
isManAvailableIO name = do
  (exitCode, _, _) <- Process.readProcess cp
  -- The exit code is actually thrown when piped to others...
  return $ exitCode == System.Exit.ExitSuccess
  where
    cp = Process.shell $ printf "man -w %s" name

-- | Converts to Command given command name and text
pageToCommandSimple :: String -> String -> Command
pageToCommandSimple name content =
  if null rootOptions
    then error ("Failed to extract information for a Command: " ++ name)
    else Postprocess.fixCommand $ Command name name rootOptions []
  where
    rootOptions = parseBlockwise content

listSubcommandsIO :: Input -> IO [Text]
listSubcommandsIO input = getSubnames <$> (pageToCommandIO name skipMan =<< getInputContent input)
  where
    name = getName input
    skipMan = getSkipMan input
    getSubnames = map (T.pack . _name) . _subcommands

getName :: Input -> String
getName (CommandInput n _) = n
getName (SubcommandInput n _ _) = n
getName (FileInput f _) = takeBaseName f
getName (JsonInput f) = takeBaseName f

getSkipMan :: Input -> Bool
getSkipMan (CommandInput _ b) = b
getSkipMan (SubcommandInput _ _ b) = b
getSkipMan (FileInput _ b) = b
getSkipMan (JsonInput _) = True
