{-# LANGUAGE DuplicateRecordFields #-}

module GenFishCompletions where

import qualified Data.List as List
import HelpParser
import Text.Printf (printf)

type Command = String

toFlag :: OptNameType -> String
toFlag LongType = "-l"
toFlag ShortType = "-s"
toFlag OldType = "-o"
toFlag _ = ""


toFishCompPart :: OptName -> String
toFishCompPart optName = flg ++ " " ++ name
  where
    name = dropWhile (== '-') (_raw optName)
    flg = toFlag (_type optName)


genFishLine :: Command -> Opt -> String
genFishLine cmd opt = line
  where
    parts = unwords $ map toFishCompPart (_names opt)
    description = _desc opt
    line = printf "complete -c %s %s -d %s" cmd parts description


genFish :: Command -> [Opt] -> String
genFish cmd opts = List.intercalate "\n" [genFishLine cmd opt | opt <- opts]
