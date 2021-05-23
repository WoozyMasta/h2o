{-# LANGUAGE DuplicateRecordFields #-}

module GenFishCompletions where

import Data.Char (toUpper)
import Data.List (isInfixOf)
import qualified Data.List as List
import Data.String.Utils (join, replace)
import HelpParser (Opt (..), OptName (..), OptNameType (..))
import Subcommand (Subcommand (..))
import Text.Printf (printf)


-- https://unix.stackexchange.com/questions/296141/how-to-use-a-special-character-as-a-normal-one-in-unix-shells
escapeSpecialSymbols :: String -> String
escapeSpecialSymbols s = List.foldl' f s symbols
  where
    f acc c = replace [c] ("\\" ++ [c]) acc
    symbols = "!?#$%&"

argToFlg :: String -> String
argToFlg "" = ""
argToFlg s
  | "FILE" `isInfixOf` strUpper = " -r"
  | "DIR" `isInfixOf` strUpper = " -r"
  | otherwise = " -x"
  where
    strUpper = map toUpper s

toFishCompPart :: OptName -> String
toFishCompPart (OptName raw t) = escapedStr
  where
    dashlessName = dropWhile (== '-') raw
    s = unwords $ filter (not . null) [toFlag t, dashlessName]
    escapedStr = escapeSpecialSymbols s

toFlag :: OptNameType -> String
toFlag LongType = "-l"
toFlag ShortType = "-s"
toFlag OldType = "-o"
toFlag _ = ""

truncateAfterPeriod :: String -> String
truncateAfterPeriod line
  | ". " `isInfixOf` line = unwords zs
  | otherwise = line
  where
    (xs, ys) = span (\w -> last w /= '.') (words line)
    zs = case ys of
      [] -> xs
      s : ss -> if criteria then xs ++ [s, extra] else xs ++ [s]
        where
          len = length s
          criteria = len >= 3 && s !! (len - 2) /= '.' && s !! (len - 3) == '.' -- like "e.g."
          extra = truncateAfterPeriod (unwords ss)

genFishLineOption :: String -> Opt -> String
genFishLineOption cmd (Opt names arg desc) = line
  where
    parts = unwords $ map toFishCompPart names
    quotedDesc = replace "'" "\\'" (truncateAfterPeriod desc)
    line = printf "complete -c %s %s -d '%s'%s" cmd parts quotedDesc (argToFlg arg)

-- | This is for a top-level option that is suppressed after a subcommand
genFishLineRootOption :: String -> [String] -> Opt -> String
genFishLineRootOption cmd subcmds (Opt names arg desc) = line
  where
    parts = unwords $ map toFishCompPart names
    quotedDesc = replace "'" "\\'" (truncateAfterPeriod desc)
    subcmdsAsStr = join " " subcmds
    cond = printf "-n \"not __fish_seen_subcommand_from %s\"" subcmdsAsStr :: String
    line = printf "complete -c %s %s %s -d '%s'%s" cmd cond parts quotedDesc (argToFlg arg)

genFishLineSubcommand :: String -> Subcommand -> String
genFishLineSubcommand cmd (Subcommand subcmd desc) = line
  where
    template = "complete -c %s -n __fish_use_subcommand -x -a %s -d '%s'"
    quotedDesc = replace "'" "\\'" desc
    line = printf template cmd subcmd quotedDesc

genFishLineSubcommandOption :: String -> String -> Opt -> String
genFishLineSubcommandOption cmd subcmd (Opt names arg desc) = line
  where
    parts = unwords $ map toFishCompPart names
    quotedDesc = replace "'" "\\'" (truncateAfterPeriod desc)
    subcmdCondition = printf "-n \"__fish_seen_subcommand_from %s\"" subcmd :: String
    line = printf "complete -c %s %s %s -d '%s'%s" cmd subcmdCondition parts quotedDesc (argToFlg arg)

genFishScript :: String -> [Opt] -> String
genFishScript cmd opts = unlines [genFishLineOption cmd opt | opt <- opts]

genFishScriptRootOptions :: String -> [String] -> [Opt] -> String
genFishScriptRootOptions cmd subcmds opts = unlines [genFishLineRootOption cmd subcmds opt | opt <- opts]

genFishScriptSubcommands :: String -> [Subcommand] -> String
genFishScriptSubcommands cmd subcmds = unlines [genFishLineSubcommand cmd sub | sub <- subcmds]

genFishScriptSubcommandOptions :: String -> String -> [Opt] -> String
genFishScriptSubcommandOptions cmd subcmd opts = unlines [genFishLineSubcommandOption cmd subcmd opt | opt <- opts]
