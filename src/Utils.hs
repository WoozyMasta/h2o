{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | get statistical mode (= the most frequently appeareing item)
module Utils where

import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.Extra (nubSort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)

getMostFrequent :: (Ord a) => [a] -> Maybe a
getMostFrequent = fmap fst . getMostFrequentWithCount

count :: (Ord a) => [a] -> [(a, Int)]
count xs = Map.toList $ Map.fromListWith (+) (map (,1) xs)

getMostFrequentWithCount :: (Ord a) => [a] -> Maybe (a, Int)
getMostFrequentWithCount [] = Nothing
getMostFrequentWithCount xs = Just (x, maxCount)
  where
    counter = count xs
    (x, maxCount) = Foldable.maximumBy (compare `on` snd) counter

convertTabsToSpaces :: Int -> Text -> Text
convertTabsToSpaces n = removeDelimiter ':' . T.unlines . map convertLine . T.lines . removeCrNewline
  where
    removeCrNewline = T.replace "\r\n" "\n"
    convertLine = List.foldl1' f . T.splitOn "\t"
    f acc t = T.concat [acc, T.replicate spaceWidth " ", t]
      where
        w = T.length acc
        offset = (w `div` n) * n + n
        spaceWidth = offset - w

removeDelimiter :: Char -> Text -> Text
removeDelimiter ch = T.intercalate "   " . T.splitOn from_
  where
    from_ = T.pack [' ', ch, ' ']

debugTag :: String
debugTag = "[debug]"

infoTag :: String
infoTag = "[info]"

warnTag :: String
warnTag = "[warn]"

traceMsgHelper :: (Show a) => String -> String -> a -> a
traceMsgHelper tag msg x = trace (unwords [tag, msg, show x, "\n"]) x

traceShowHelper :: (Show b) => String -> String -> b -> a -> a
traceShowHelper tag msg var = trace (unwords [tag, msg, show var ++ "\n"])

debugMsg :: (Show a) => String -> a -> a
debugMsg = traceMsgHelper debugTag

infoMsg :: (Show a) => String -> a -> a
infoMsg = traceMsgHelper infoTag

warnMsg :: (Show a) => String -> a -> a
warnMsg = traceMsgHelper warnTag

debugShow :: (Show b) => String -> b -> a -> a
debugShow = traceShowHelper debugTag

infoShow :: (Show b) => String -> b -> a -> a
infoShow = traceShowHelper infoTag

warnShow :: (Show b) => String -> b -> a -> a
warnShow = traceShowHelper warnTag

traceHelper :: String -> String -> a -> a
traceHelper tag msg = trace (unwords [tag, show msg])

debugTrace :: String -> a -> a
debugTrace = traceHelper debugTag

infoTrace :: String -> a -> a
infoTrace = traceHelper infoTag

warnTrace :: String -> a -> a
warnTrace = traceHelper warnTag

traceIf :: (a -> Bool) -> (a -> String) -> a -> a
traceIf check run x
  | check x = trace (run x) x
  | otherwise = x

-- | hyphen-aware unlines
-- Here supporting hypen as in unicode \8208 (decimal) = \2010 (hex)
-- https://unicode-table.com/en/2010/
smartUnwords :: [String] -> String
smartUnwords =
  foldr1 f
  where
    f "" acc = acc
    f s "" = s
    f s acc
      | c == '-' || c == '\8208' = init s ++ acc
      | c == ' ' = s ++ acc
      | otherwise = s ++ (' ' : acc)
      where
        c = last s

-- | convert strictly-increasing ints to a list of left-inclusive right-exclusive ranges
-- toRanges [1,2,3,4,6,9,10] == [(1, 5), (6, 7), (9, 11)]
-- assert the input is sorted
toRanges :: [Int] -> [(Int, Int)]
toRanges = foldr f []
  where
    f x [] = [(x, x + 1)]
    f x ((a, b) : rest)
      | x + 1 == a = (x, b) : rest
      | otherwise = (x, x + 1) : (a, b) : rest

-- | convert from left-inclusive right-exclusive ranges to a list of integers
fromRanges :: [(Int, Int)] -> [Int]
fromRanges = nubSort . concatMap fromRange

fromRange :: (Int, Int) -> [Int]
fromRange (a, b) = take (b - a) [a, a + 1 ..]

getParagraph :: [String] -> (Int, Int) -> String
getParagraph xs range = unlines $ map (xs !!) indices
  where
    indices = fromRange range

-- | check if the string starts with non-space char `c`
startsWithChar :: Char -> String -> Bool
startsWithChar c s = not (null ss) && head ss == c
  where
    ss = dropWhile (`elem` (" \t" :: String)) s

-- | check if the string starts with dash - possibly after spaces and tabs
startsWithDash :: String -> Bool
startsWithDash = startsWithChar '-'

-- | check if the string starts with -- possibly after spaces and tabs
startsWithDoubleDash :: String -> Bool
startsWithDoubleDash s = case ss of
  "" -> False
  [_] -> False
  c1 : c2 : _ -> c1 == '-' && c2 == '-'
  where
    ss = dropWhile (== ' ') s

startsWithSingleDash :: String -> Bool
startsWithSingleDash s = case ss of
  "" -> False
  [_] -> False
  c1 : c2 : _ -> c1 == '-' && c2 /= '-'
  where
    ss = dropWhile (== ' ') s

startsWithLongOption :: String -> Bool
startsWithLongOption s = startsWithDoubleDash s && length ss >= 3 && ss !! 2 /= ' '
  where
    ss = dropWhile (== ' ') s

startsWithShortOrOldOption :: String -> Bool
startsWithShortOrOldOption s = startsWithDash s && length ss >= 2 && ss !! 1 /= ' '
  where
    ss = dropWhile (== ' ') s

-- | A speculative criteria for non-critical purposes
mayContainOptions :: [Text] -> Bool
mayContainOptions = (>= 2) . length . filter (T.isPrefixOf "-" . T.stripStart)

-- | Another speculative criteria for non-critical purposes
mayContainSubcommands :: [Text] -> Bool
mayContainSubcommands = (>= 4) . length . filter ((>= 2) . length . T.words) . filter (\t -> " " `T.isPrefixOf` t || "\t" `T.isPrefixOf` t) . filter (not . T.null)

getTopLevelHeadingIndices :: [Text] -> [Int]
getTopLevelHeadingIndices xs
  | null xs = []
  | otherwise = [idx | (idx, indentation) <- zip [0 ..] indentations, indentation == minval]
  where
    indentations = map (\x -> if T.null x then 80 else (T.length . T.takeWhile (== ' ')) x) xs
    minval = List.minimum indentations

splitByTopHeaders :: Text -> [Text]
splitByTopHeaders text
  | T.null text = []
  | any startsWithLongOption headingsStr || any startsWithShortOrOldOption headingsStr = [text]
  | otherwise = map T.unlines $ splitsAt xs headingIndices
  where
    xs = T.lines text
    headingIndices = getTopLevelHeadingIndices xs
    headings = map (xs !!) headingIndices
    headingsStr = map T.unpack headings

dropUsage :: Text -> Text
dropUsage text
  | null rest = text
  | otherwise = T.concat rest
  where
    xs = splitByTopHeaders text
    isUsageBlock = ("usage" `T.isPrefixOf`) . T.toLower . T.stripStart
    rest = filter (not . isUsageBlock) xs

-- | A speculative criteria for non-critical purposes
mayContainUseful :: Text -> Bool
mayContainUseful text = length xs >= 4 && (mayContainOptions xs || mayContainSubcommands xs)
  where
    xs = filter (not . ("error" `T.isPrefixOf`) . T.toLower . T.stripStart) . T.lines $ dropUsage text

-- | splitsAt ... like Data.List.splitAt but multiple indices
splitsAt :: [a] -> [Int] -> [[a]]
splitsAt xs ns = reverse $ filter (not . null) $ List.unfoldr f (xs, reverse ns)
  where
    f :: ([a], [Int]) -> Maybe ([a], ([a], [Int]))
    f ([], _) = Nothing
    f (ys, []) = Just (ys, ([], []))
    f (ys, k : ks) = Just (latter, (former, ks))
      where
        (former, latter) = List.splitAt k ys

topTenPercentile :: (Ord a) => [a] -> a
topTenPercentile [] = error "Cannot compute percentile against null!"
topTenPercentile xs = sortedXs !! idx
  where
    n = length xs
    idx = fromInteger $ floor (fromIntegral (n - 1) * 0.9 :: Rational) :: Int
    sortedXs = List.sort xs

isBracketed :: Text -> Bool
isBracketed text
  | T.length text <= 1 = False
  | otherwise = (T.head text, T.last text) `elem` bracketPairs

hasClosedBrackets :: Char -> Char -> Text -> Bool
hasClosedBrackets bra ket = helper 0
  where
    helper :: Int -> Text -> Bool
    helper acc txt
      | T.null txt = acc == 0
      | acc < 0 = False
      | otherwise = helper (acc + diff) rest
      where
        c = T.head txt
        rest = T.tail txt
        diff
          | c == bra = 1
          | c == ket = -1
          | otherwise = 0

hasMatchingBrackets :: Text -> Bool
hasMatchingBrackets text = hasBra && allCleared
  where
    (bras, _) = unzip bracketPairs
    hasBra = any (\b -> T.singleton b `T.isInfixOf` text) bras
    allCleared = and [hasClosedBrackets bra ket text | (bra, ket) <- bracketPairs]

bracketPairs :: [(Char, Char)]
bracketPairs =
  [ ('{', '}'),
    ('<', '>'),
    ('[', ']'),
    ('(', ')')
  ]
