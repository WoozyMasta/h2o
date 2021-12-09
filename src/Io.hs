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

run :: ConfigOrVersion -> IO Text
run Version = return (T.concat ["h2o ", Constants.versionStr, "\n"])
run (C_ (Config input _ isExportingJSON isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly isShallowOnly))
  | isExportingJSON = Utils.warnTrace "io: Deprecated: Use --format json instead" $ run (C_ (Config input Json False False False False isShallowOnly))
  | isConvertingTabsToSpaces = infoTrace "io: Converting tags to spaces...\n" $ T.pack <$> getInputContent input
  | isListingSubcommands = infoTrace "io: Listing subcommands...\n" $ T.unlines . map T.pack <$> listSubcommandsIO name
  | isPreprocessOnly = infoTrace "io: processing (option+arg, description) splitting only" $ T.pack . formatStringPairs . preprocessBlockwise <$> getInputContent input
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])
    name = case input of
      CommandInput n -> n
      SubcommandInput n _ -> n
      FileInput f -> takeBaseName f
      JsonInput f -> takeBaseName f
run (C_ (Config input@(FileInput f) format _ _ _ _ isShallowOnly))
  | isShallowOnly = infoTrace "io: processing just the file" $ toScript format . pageToCommandSimple name <$> contentIO
  | otherwise = infoTrace "io: processing the file and more" $ toScript format <$> (pageToCommandIO name =<< contentIO)
  where
    name = takeBaseName f
    contentIO = getInputContent input
run (C_ (Config input@(CommandInput name) format _ _ _ _ isShallowOnly))
  | isShallowOnly = toScript format . pageToCommandSimple name <$> getInputContent input
  | otherwise = toScript format <$> toCommandIO name
run (C_ (Config input@(SubcommandInput name subname) format _ _ _ _ _)) =
  toScript format . pageToCommandSimple nameSubname <$> getInputContent input
  where
    nameSubname = name ++ "-" ++ subname
run (C_ (Config input@(JsonInput _) format _ _ _ _ _)) = do
  content <- TLE.encodeUtf8 . TL.pack <$> getInputContent input
  let cmdMay = Aeson.decode content :: Maybe Command
  let commandIO =
        case cmdMay of
          Nothing -> error "Cannot decode JSON!"
          Just c -> return c
  toScript format <$> commandIO

getManSub :: String -> String -> IO Text
getManSub name subname = getMan (name ++ "-" ++ subname)

getManAndHelpSub :: String -> String -> IO Text
getManAndHelpSub name subname = do
  content <- getManSub name subname
  if T.null content
    then do
      content2 <- getHelpSub name subname
      if T.null content2
        then error ("io: Neither help or man pages available: " ++ name ++ "-" ++ subname)
        else infoTrace "io: Using help for subcommand" $ return content2
    else infoTrace "io: Using manpage for subcommand" $ return content

-- | Following implementation takes more than 2x longer than the above... WHY??
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

getHelpSub :: String -> String -> IO Text
getHelpSub name subname = getHelpTemplate name [[subname, "--help"], ["help", subname], [subname, "-help"], [subname, "-h"]]

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
getInputContent (SubcommandInput name subname) =
  T.unpack . Utils.dropUsage . Utils.convertTabsToSpaces 8 <$> getManAndHelpSub name subname
getInputContent (CommandInput name) =
  T.unpack . Utils.dropUsage . Utils.convertTabsToSpaces 8 <$> getManAndHelp name
getInputContent (FileInput f) =
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

toCommandIO :: String -> IO Command
toCommandIO name = do
  content <- getInputContent (CommandInput name)
  pageToCommandIO name content

pageToCommandIO :: String -> String -> IO Command
pageToCommandIO name content = do
  subcmdOptsPairs <- subcmdOptsPairsM
  if null rootOptions && null subcmdOptsPairs
    then error ("Failed to extract information for a Command: " ++ name)
    else return $ Postprocess.fixCommand $ toCommand name name rootOptions subcmdOptsPairs
  where
    sub2pair (Subcommand s1 s2) = (s1, s2)
    pair2sub = uncurry Subcommand
    uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair
    rootOptions = parseBlockwise content
    subcmdCandidates =
      infoMsg "subcommand candidates : \n" $ uniqSubcommands (parseSubcommand content)
    toSubcmdOptPair useMan sub = do
      page <-
        Utils.dropUsage . Utils.convertTabsToSpaces 8
          <$> if useMan then getManSub name (_cmd sub) else getHelpSub name (_cmd sub)
      let criteria = not (T.null page) && page /= T.pack content
      return ((sub, parseBlockwise (T.unpack page)), criteria)
    pairsIO = do
      !isManAvailable <- isManAvailableIO name
      mapM (toSubcmdOptPair isManAvailable) subcmdCandidates
    subcmdOptsPairsM = map fst . filter snd <$> pairsIO

isManAvailableIO :: String -> IO Bool
isManAvailableIO name = do
  (exitCode, _, _) <- Process.readProcess cp
  -- The exit code is actually thrown when piped to others...
  return $ exitCode == System.Exit.ExitSuccess
  where
    cp = Process.shell $ printf "man -w %s" name

-- Convert to Command given command name and text
pageToCommandSimple :: String -> String -> Command
pageToCommandSimple name content =
  if null rootOptions
    then error ("Failed to extract information for a Command: " ++ name)
    else Postprocess.fixCommand $ Command name name rootOptions []
  where
    rootOptions = parseBlockwise content

listSubcommandsIO :: String -> IO [String]
listSubcommandsIO name = getSubnames <$> toCommandIO name

getSubnames :: Command -> [String]
getSubnames = map _name . _subcommands
