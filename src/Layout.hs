{-# LANGUAGE DuplicateRecordFields #-}

module Layout where

import qualified Data.List as List
import Debug.Trace (trace)

-- get width when viewed with tabsize 8
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

-- | get line number and column that starts with
-- λ> getOptionLocations " \n\n  \t  --option here"
-- [(2, 10)]
getOptionLocations :: String -> [(Int, Int)]
getOptionLocations s = [(n, getCol x) | (n, x) <- pairs, startsWithDash x]
  where
    pairs = zip [(0 :: Int) ..] (lines s)
    getCol = getWidth . takeWhile (/= '-')
