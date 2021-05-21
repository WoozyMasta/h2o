module Subcommand where

import Control.Monad (liftM2)
import HelpParser (newline, optWord, singleSpace, skip, word)
import Text.ParserCombinators.ReadP
import Utils (getMostFrequent)

type Layout = (Int, Int)

data Subcommand = Subcommand
  { _cmd :: String,
    _desc :: String
  }

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
    pairs = map firstTwoWordsLoc xs
    first = getMostFrequent [a | (a, _) <- pairs, a >= 0]
    second = getMostFrequent [b | (_, b) <- pairs, b >= 0]

getAlignedLines :: String -> [String]
getAlignedLines s = case layout of
  Just lay -> filter (\line -> firstTwoWordsLoc line == lay) xs
  _ -> []
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
