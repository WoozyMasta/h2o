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

genFishLineOption :: Command -> Opt -> String
genFishLineOption cmd (Opt names arg desc) = line
  where
    parts = unwords $ map toFishCompPart names
    quotedDesc = replace "'" "\\'" (truncateAfterPeriod desc)
    argFlag = case arg of
      "" -> ""
      arg | "FILE" `isInfixOf` map toUpper arg -> " -r"
      _ -> " -x"
    line = printf "complete -c %s %s -d '%s'%s" cmd parts quotedDesc argFlag

    toFishCompPart :: OptName -> String
    toFishCompPart (OptName raw t) = toFlag t ++ " " ++ dashlessName
      where
        dashlessName = dropWhile (== '-') raw

    toFlag :: OptNameType -> String
    toFlag LongType = "-l"
    toFlag ShortType = "-s"
    toFlag OldType = "-o"
    toFlag _ = ""

truncateAfterPeriod :: String -> String
truncateAfterPeriod s
  | ". " `isInfixOf` s = unwords zs
  | otherwise = s
  where
    (xs, ys) = span (\w -> last w /= '.') (words s)
    zs = xs ++ [head ys]

genFishLineSubcommand :: Command -> Subcommand -> String
genFishLineSubcommand cmd (Subcommand subcmd desc) = line
  where
    template = "complete -c %s -n __fish_use_subcommand -a %s -d '%s'"
    quotedDesc = replace "'" "\\'" desc
    line = printf template cmd subcmd quotedDesc

genFishScript :: Command -> [Opt] -> String
genFishScript cmd opts = optionPart
  where
    optionPart = unlines [genFishLineOption cmd opt | opt <- opts]
