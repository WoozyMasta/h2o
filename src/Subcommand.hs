module Subcommand where

import Control.Monad (liftM2)
import qualified Data.Maybe as Maybe
import HelpParser (newline, optWord, singleSpace, skip, word)
import Text.ParserCombinators.ReadP
import Utils (convertTabsToSpaces, debugMsg, getMostFrequent, startsWithChar)

type Layout = (Int, Int)

data Subcommand = Subcommand
  { _cmd :: String,
    _desc :: String
  }

instance Show Subcommand where
  show (Subcommand cmd desc) = cmd ++ "\t\t(" ++ desc ++ ")"

-- | Returns location of first two words:
-- -1 if the first or the second word unavailable
--    firstTwoWordsLoc "  hello" == (2, -1)
--    firstTwoWordsLoc "       " == (-1, -1)
firstTwoWordsLoc :: String -> (Int, Int)
firstTwoWordsLoc line = (firstLoc, secondLoc)
  where
    (spaces, rest0) = span (' ' ==) line
    (w, rest1) = span (' ' /=) rest0
    (midSpaces, rest2) = span (' ' ==) rest1
    firstLoc = if null w then -1 else length spaces
    secondLoc = if null rest2 then -1 else firstLoc + length w + length midSpaces

getLayout :: [String] -> Maybe Layout
getLayout xs = liftM2 (,) first second
  where
    pairs = debugMsg "first two word locations:" $ filter (\(a, b) -> a > 0 && b >= a + 6) $ map firstTwoWordsLoc xs
    second = getMostFrequent [b | (_, b) <- pairs]
    first = getMostFrequent [a | (a, _) <- pairs, a < Maybe.fromMaybe 50 second]

getAlignedLines :: String -> [String]
getAlignedLines s = case layout of
  Just lay -> filter (\line -> firstTwoWordsLoc line == lay) xs
  _ -> []
  where
    xs = lines s
    layout = debugMsg "subcommand layout :" $ getLayout xs

subcommand :: ReadP Subcommand
subcommand = do
  skipSpaces
  cmd <- optWord
  skipSpaces
  ss <- sepBy1 word singleSpace
  skip (munch (`elem` " \t"))
  skip newline <++ eof
  let desc = unwords ss
  return (Subcommand cmd desc)

removeOptionLines :: String -> String
removeOptionLines content = unlines xs
  where
    f s =
      not (null s)
        && (not . startsWithChar '-' $ s)
        && (not . startsWithChar '[' $ s)
        && (head s == ' ')
    xs = filter f (lines content)

parseSubcommand :: String -> [Subcommand]
parseSubcommand content = results
  where
    s = convertTabsToSpaces 8 content
    s' = removeOptionLines s
    xs = getAlignedLines s'
    results = (map (fst . last) . filter (not . null)) [readP_to_S subcommand x | x <- xs]
