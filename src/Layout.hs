{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace (trace, traceShow)

-- | Location is defined by (row, col) order
type Location = (Int, Int)

-- [TODO] memoise the calls
-- https://stackoverflow.com/questions/3208258/memoization-in-haskell

-- | Get widLocation is defined byth when viewed with tabsize 8
getWidth :: String -> Int
getWidth s
  | '\t' `elem` s = List.foldl' f 0 s
  | otherwise = length s
  where
    n = 8
    f acc '\t' = (acc `div` n) * n + n
    f acc _ = acc + 1

startsWithDash :: String -> Bool
startsWithDash s = not (null ss) && (head ss == '-')
  where
    ss = dropWhile (`elem` " \t") s

getNonblankLocationTemplate :: (String -> Bool) -> String -> [Location]
getNonblankLocationTemplate f s = [(i, getCol x) | (i, x) <- enumLines, f x]
  where
    enumLines = zip [(0 :: Int) ..] (lines s)
    getCol = getWidth . takeWhile (`elem` " \t")

-- | Get location that starts with '-'
-- λ> getOptionLocations " \n\n  \t  --option here"
-- [(2, 10)]
getOptionLocations :: String -> [Location]
getOptionLocations = getNonblankLocationTemplate startsWithDash

-- | Get locations of lines NOT starting with dash
getNonoptLocations :: String -> [Location]
getNonoptLocations = getNonblankLocationTemplate (not . startsWithDash)

getMostFrequent :: (Ord a) => [a] -> a
getMostFrequent xs = x
  where
    counter = Map.toList $ Map.fromListWith (+) (map (,1) xs)
    (x, maxCount) = Foldable.maximumBy (compare `on` snd) counter

-- | Assume the most frequent column width as the horizontal offset of options
getOptionOffset :: String -> Int
getOptionOffset s = traceShow droppedOptionLines res
  where
    locs = getOptionLocations s
    res = getMostFrequent (map snd locs)
    locLinePairs = zip locs (lines s)
    droppedOptionLines = [x | ((_, c), x) <- locLinePairs, c /= res]

----------------------------------------
-- For 3-pane layout (short-option   long-option   description)

isThreePaneLayout :: String -> Bool
isThreePaneLayout s = getOptionOffset s == getLongOptionOffset s

startsWithDoubleDash :: String -> Bool
startsWithDoubleDash s = case ss of
  "" -> False
  [c] -> False
  c1 : c2 : cs -> c1 == '-' && c2 == '-'
  where
    ss = dropWhile (`elem` " \t") s

getLongOptionLocations :: String -> [Location]
getLongOptionLocations = getNonblankLocationTemplate startsWithDoubleDash

getLongOptionOffset :: String -> Int
getLongOptionOffset s = traceShow droppedOptionLines res
  where
    locs = getLongOptionLocations s
    res = getMostFrequent (map snd locs)
    locLinePairs = zip locs (lines s)
    droppedOptionLines = [x | ((_, c), x) <- locLinePairs, c /= res]

----------------------------------------

-- | [FIXME] the argument is too rough...
descriptionOffset :: String -> Int
descriptionOffset s = res
  where
    locs = getNonoptLocations s
    optionOffset = getOptionOffset s
    cols = [x | (_, x) <- locs, optionOffset + 3 <= x]
    res = getMostFrequent cols
