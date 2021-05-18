{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- | get statistical mode (= the most frequently appeareing item)
module Utils where

import Control.Exception (assert)
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map

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
