{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- | get statistical mode (= the most frequently appeareing item)
module Utils where

import Control.Exception (assert)
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.Extra (nubSort)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.Printf (printf)

getMostFrequent :: (Ord a) => [a] -> Maybe a
getMostFrequent = fmap fst . getMostFrequentWithCount

getMostFrequentWithCount :: (Ord a) => [a] -> Maybe (a, Int)
getMostFrequentWithCount [] = Nothing
getMostFrequentWithCount xs = Just (x, maxCount)
  where
    counter = Map.toList $ Map.fromListWith (+) (map (,1) xs)
    (x, maxCount) = Foldable.maximumBy (compare `on` snd) counter

-- | Covert tabs to spaces
convertTabsToSpaces :: Int -> String -> String
convertTabsToSpaces n s = unlines $ map convertLine $ lines s
  where
    convertLine x
      | '\t' `elem` x = reverse $ List.foldr f "" (reverse x)
      | otherwise = x
    f '\t' acc = replicate spaceWidth ' ' ++ acc
      where
        w = length acc
        offset = (w `div` n) * n + n
        spaceWidth = offset - w
    f c acc = c : acc

debugMsg :: (Show a) => String -> a -> a
debugMsg msg x = trace (printf ("[debug] " ++ msg ++ " %s\n") (show x)) x

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
fromRanges = nubSort . concatMap f
  where
    f (a, b) = take (b - a) [a, a + 1 ..]