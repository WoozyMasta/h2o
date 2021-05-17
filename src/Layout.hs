{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import Control.Exception (assert)
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.String.Utils (join, rstrip, split)
import Debug.Trace (trace, traceShow)
import Utils (getMostFrequent)

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

-- | check if the string starts with dash - possibly after spaces and tabs
startsWithDash :: String -> Bool
startsWithDash s = not (null ss) && (head ss == '-')
  where
    ss = dropWhile (`elem` " \t") s

-- | a helper function
_getNonblankLocationTemplate :: (String -> Bool) -> String -> [Location]
_getNonblankLocationTemplate f s = [(i, getCol x) | (i, x) <- enumLines, f x]
  where
    enumLines = zip [(0 :: Int) ..] (lines s)
    getCol = getWidth . takeWhile (`elem` " \t")

-- | Get location that starts with '-'
-- λ> getOptionLocations " \n\n  \t  --option here"
-- [(2, 10)]
getOptionLocations :: String -> [Location]
getOptionLocations = _getNonblankLocationTemplate startsWithDash

-- | Get locations of lines NOT starting with dash
getNonoptLocations :: String -> [Location]
getNonoptLocations = _getNonblankLocationTemplate (not . startsWithDash)

_getOffsetHelper :: (String -> [Location]) -> String -> Maybe Int
_getOffsetHelper cond s = traceShow droppedOptionLines res
  where
    locs = cond s
    xs = map snd locs
    res = getMostFrequent xs
    locLinePairs = zip locs (lines s)
    droppedOptionLines = [x | ((_, c), x) <- locLinePairs, c /= fromJust res]

-- | get presumed horizontal offset of options lines
getOptionOffset :: String -> Maybe Int
getOptionOffset = _getOffsetHelper getOptionLocations

----------------------------------------
-- For 3-pane layout (short-option   long-option   description)

-- | check if the layout appears 3-pane
isThreePaneLayout :: String -> Bool
isThreePaneLayout s = getOptionOffset s == getLongOptionOffset s

-- | check if the string starts with -- possibly after spaces and tabs
startsWithDoubleDash :: String -> Bool
startsWithDoubleDash s = case ss of
  "" -> False
  [c] -> False
  c1 : c2 : _ -> c1 == '-' && c2 == '-'
  where
    ss = dropWhile (`elem` " \t") s

-- | get location of long options
getLongOptionLocations :: String -> [Location]
getLongOptionLocations = _getNonblankLocationTemplate startsWithDoubleDash

-- | get presumed horizontal offset of long options
getLongOptionOffset :: String -> Maybe Int
getLongOptionOffset = _getOffsetHelper getLongOptionLocations

----------------------------------------

-- | [FIXME] the argument is too rough implementing only 1)...
--
-- There are two independent ways to guess the horizontal offset of descriptions
--   1) A description line may be simply offset by space
--   2) A description line may appear following an option
--      Focus on the pattern that the description and the options+args
--      may be split by 3 spaces
descriptionOffset :: String -> Maybe Int
descriptionOffset s =
  assert ('\t' `notElem` s) res
  where
    locs = getNonoptLocations s
    optionOffset = getOptionOffset s
    cols = [x | (_, x) <- locs, fromJust optionOffset + 3 <= x]
    res = getMostFrequent cols
