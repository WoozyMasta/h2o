{-# LANGUAGE DuplicateRecordFields #-}

module GenFishCompletions where

import Data.Char (toUpper)
import Data.List (isInfixOf)
import qualified Data.List as List
import Data.String.Utils (replace)
import HelpParser
import Subcommand
import Text.Printf (printf)

type Command = String

-- https://unix.stackexchange.com/questions/296141/how-to-use-a-special-character-as-a-normal-one-in-unix-shells
escapeSpecialSymbols :: String -> String
escapeSpecialSymbols s = List.foldl' f s symbols
  where
    f acc c = replace [c] ("\\" ++ [c]) acc
    symbols = "!?#$%&"

genFishLineOption :: Command -> Opt -> String
genFishLineOption cmd (Opt names arg desc) = line
  where
    parts = unwords $ map toFishCompPart names
    quotedDesc = replace "'" "\\'" (truncateAfterPeriod desc)
    line = printf "complete -c %s %s -d '%s'%s" cmd parts quotedDesc (argToFlg arg)

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
      s : _ -> if criteria then xs ++ [s, extra] else xs ++ [s]
        where
          len = length s
          criteria = len >= 3 && s !! (len - 2) /= '.' && s !! (len - 3) == '.' -- like "e.g."
          extra = truncateAfterPeriod (unwords ys)

genFishLineSubcommand :: Command -> Subcommand -> String
genFishLineSubcommand cmd (Subcommand subcmd desc) = line
  where
    template = "complete -c %s -n __fish_use_subcommand -a %s -d '%s'"
    quotedDesc = replace "'" "\\'" desc
    line = printf template cmd subcmd quotedDesc

genFishLineSubcommandOption :: Command -> Command -> Opt -> String
genFishLineSubcommandOption cmd subcmd (Opt names arg desc) = line
  where
    parts = unwords $ map toFishCompPart names
    quotedDesc = replace "'" "\\'" (truncateAfterPeriod desc)
    subcmdCondition = printf "-n \"__fish_seen_subcommand_from %s\"" subcmd :: String
    line = printf "complete -c %s %s %s -d '%s'%s" cmd subcmdCondition parts quotedDesc (argToFlg arg)

genFishScript :: Command -> [Opt] -> String
genFishScript cmd opts = unlines [genFishLineOption cmd opt | opt <- opts]

genFishScriptUnderSubcommand :: Command -> Command -> [Opt] -> String
genFishScriptUnderSubcommand cmd subcmd opts = unlines [genFishLineSubcommandOption cmd subcmd opt | opt <- opts]
