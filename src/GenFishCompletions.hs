{-# LANGUAGE DuplicateRecordFields #-}

module GenFishCompletions where

import qualified Data.List as List
import HelpParser
import Subcommand
import Data.List (isInfixOf)
import Data.Char (toUpper)
import Text.Printf (printf)
import Data.String.Utils (replace)

type Command = String

genFishLineOption :: Command -> Opt -> String
genFishLineOption cmd (Opt names arg desc) = line
  where
    parts = unwords $ map toFishCompPart names
    quotedDesc = replace "'" "\\'" desc
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
