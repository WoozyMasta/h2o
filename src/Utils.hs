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

getMostFrequentWithCount :: (Ord a) => [a] -> Maybe (a, Int)
getMostFrequentWithCount [] = Nothing
getMostFrequentWithCount xs = Just (x, maxCount)
  where
    counter = Map.toList $ Map.fromListWith (+) (map (,1) xs)
    (x, maxCount) = Foldable.maximumBy (compare `on` snd) counter

convertTabsToSpaces :: Int -> String -> String
convertTabsToSpaces n = T.unpack . removeDelimiter ':' . T.unlines . map convertLine . T.lines . removeCrNewline . T.pack
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

-- | A speculative criteria for non-critical purposes
mayContainsOptions :: String -> Bool
mayContainsOptions = (>= 2) . List.foldr (max . length) 0 . List.group . map startsWithDash . lines

-- | Another speculative criteria for non-critical purposes
mayContainsSubcommands :: String -> Bool
mayContainsSubcommands = (>= 4) . List.foldr (max . length) 0 . List.group . map (startsWithChar ' ') . lines

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
