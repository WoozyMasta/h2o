module Subcommand where

import Control.Monad (liftM2)
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)
import HelpParser (newline, optWord, singleSpace, skip, word)
import Layout (getDescriptionOffset)
import Text.ParserCombinators.ReadP
    ( ReadP, (<++), eof, munch, readP_to_S, sepBy1, skipSpaces )
import Type (Subcommand (..))
import Utils (convertTabsToSpaces, debugMsg, getMostFrequent, startsWithChar)

type Layout = (Int, Int)

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

getLayoutMaybe :: [String] -> Maybe Layout
getLayoutMaybe xs = liftM2 (,) first second
  where
    pairs = debugMsg "first two word locations:" $ filter (\(a, b) -> a > 0 && b >= a + 6) $ map firstTwoWordsLoc xs
    second = getMostFrequent [b | (_, b) <- pairs]
    first = getMostFrequent [a | (a, _) <- pairs, a < Maybe.fromMaybe 50 second]

getAlignedLines :: String -> [String]
getAlignedLines s = case (layoutMay, offsetMay) of
  (Just lay, Just offset) ->
    if offset <= fst lay
      then trace "[subcommand] discard layout because of description offset" []
      else filter (\line -> firstTwoWordsLoc line == lay) xs
  (Just lay, Nothing) -> filter (\line -> firstTwoWordsLoc line == lay) xs
  _ -> []
  where
    xs = lines s
    layoutMay = debugMsg "subcommand layout :" $ getLayoutMaybe xs
    offsetMay = getDescriptionOffset s

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
