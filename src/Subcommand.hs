module Subcommand where

import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.Map.Strict as Map
import HelpParser
import Text.ParserCombinators.ReadP

type Layout = (Int, Int)

data Subcommand = Subcommand
  { _cmd :: String,
    _desc :: String
  }

firstTwoWordsLoc :: String -> (Int, Int)
firstTwoWordsLoc line = (firstLoc, secondLoc)
  where
    (spaces, rest0) = span (' ' ==) line
    (word, rest1) = span (' ' /=) rest0
    (midSpaces, rest2) = span (' ' ==) rest1
    firstLoc = if null word then -1 else length spaces
    secondLoc = if null rest2 then -1 else firstLoc + length word + length midSpaces

getMostFrequent :: (Ord a) => [a] -> a
getMostFrequent xs = x
  where
    counter = Map.toList $ Map.fromListWith (+) (map (\x -> (x, 1)) xs)
    (x, maxCount) = Foldable.maximumBy (compare `on` snd) counter

getLayout :: [String] -> Layout
getLayout xs = (first, second)
  where
    pairs = map firstTwoWordsLoc xs
    first = getMostFrequent [a | (a, _) <- pairs, a >= 0]
    second = getMostFrequent [b | (_, b) <- pairs, b >= 0]

getAlignedLines :: String -> [String]
getAlignedLines s = filter (\line -> firstTwoWordsLoc line == layout) xs
  where
    xs = lines s
    layout = getLayout xs

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

startingWithDash :: String -> Bool
startingWithDash s = if null text then False else head text == '-'
  where
    text = dropWhile (' ' ==) s

removeOptionLines :: String -> String
removeOptionLines s = unlines xs
  where
    xs = filter (not . startingWithDash) (lines s)

parseSubcommand :: String -> [Subcommand]
parseSubcommand s = results
  where
    s' = removeOptionLines s
    xs = getAlignedLines s'
    results = (map (fst . last) . filter (not . null)) [readP_to_S subcommand x | x <- xs]
