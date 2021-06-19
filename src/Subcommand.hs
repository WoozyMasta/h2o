module Subcommand where

import Control.Monad (liftM2)
import qualified Data.Maybe as Maybe
import HelpParser (alphanumChars, newline, singleSpace, skip, word)
import Layout (getDescriptionOffset)
import Text.ParserCombinators.ReadP
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

getLayoutMaybe :: [String] -> Int -> Maybe Layout
getLayoutMaybe xs offset = liftM2 (,) first second
  where
    pairs =
      debugMsg "first two word locations:" $
        filter (\(a, b) -> a > 0 && b >= a + 6 && a <= offset) $ map firstTwoWordsLoc xs
    second = getMostFrequent [b | (_, b) <- pairs]
    first = getMostFrequent [a | (a, _) <- pairs, a < Maybe.fromMaybe 50 second]

getAlignedLines :: String -> [String]
getAlignedLines s = case layoutMay of
  Just lay -> filter (\line -> firstTwoWordsLoc line == lay) xs
  _ -> []
  where
    offsetMay = getDescriptionOffset s
    offset = Maybe.fromMaybe 50 offsetMay
    xs = filter removeOptionLine (lines s)
    layoutMay = debugMsg "[subcommand] layout :" $ getLayoutMaybe xs offset

lowercase :: String
lowercase = "abcdefghijklmnopqrstuvwxyz"

isAlphanumOrDash :: Char -> Bool
isAlphanumOrDash c = c `elem` ('-':alphanumChars)

subcommandWord :: ReadP String
subcommandWord = do
  x <- satisfy $ \c -> c `elem` lowercase
  xs <- munch isAlphanumOrDash
  -- For example docker run --help has "--docker*"
  _ <- char '*' <++ pure '*'
  return (x : xs)

subcommand :: ReadP Subcommand
subcommand = do
  skipSpaces
  cmd <- subcommandWord
  skipSpaces
  _ <- char ':' <++ pure 'x'
  skipSpaces
  ss <- sepBy1 word singleSpace
  skip (munch (`elem` " \t"))
  skip newline <++ eof
  let desc = unwords ss
  return (Subcommand cmd desc)

removeOptionLine :: String -> Bool
removeOptionLine s =
  not (null s)
    && (head s == ' ')
    && (not . startsWithChar '-' $ s)
    && (not . startsWithChar '[' $ s)

parseSubcommand :: String -> [Subcommand]
parseSubcommand content = results
  where
    s = convertTabsToSpaces 8 content
    xs = getAlignedLines s
    results = (map (fst . last) . filter (not . null)) [readP_to_S subcommand x | x <- xs]
