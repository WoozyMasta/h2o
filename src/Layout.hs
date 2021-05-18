{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import Control.Exception (assert)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.String.Utils (join, rstrip, split)
import Debug.Trace (trace, traceShow, traceShowId)
import Utils (convertTabsToSpaces, getMostFrequent, getMostFrequentWithCount)
import Text.Printf (printf)

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
    f acc '\t' = acc `div` n * n + n
    f acc _ = acc + 1

-- | check if the string starts with dash - possibly after spaces and tabs
startsWithDash :: String -> Bool
startsWithDash s = not (null ss) && head ss == '-'
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
_getOffsetHelper cond s = traceMessage res
  where
    locs = cond s
    xs = map snd locs
    res = getMostFrequent xs
    droppedOptionLinesInfo = unlines [(printf "[Dropped] %03d: %s" r (lines s !! r) :: String) | (r, c) <- locs, Just c /= res]
    traceMessage = trace droppedOptionLinesInfo

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

-- There are two independent ways to guess the horizontal offset of descriptions
--   1) A description line may be simply offset by space
--   2) A description line may appear following an option
--      	... from the pattern that the description and the options+args
--      	may be separated by 3 or more spaces
-- Returns Nothing if 1 and 2 disagrees, or no information in 1 and 2
--
getDescriptionOffset :: String -> Maybe Int
getDescriptionOffset s =
  case (descOffsetWithCountSimple s, descOffsetWithCountInOptionLines s) of
    (Nothing, Nothing) -> trace "[WARNING] Retrieved absolutely zero information" Nothing
    (Nothing, q) -> trace "Descriptions always appear in the lines with options" $ fmap fst q
    (p, Nothing) -> trace "Descriptions never appear in the lines with options" $ fmap fst p
    (Just (x1, c1), Just (x2, c2))
      | x1 == x2 -> Just x1
      | otherwise -> traceMessage Nothing
      where
        traceMessage = trace (printf "[WARNING] Disagreement (x1, c1, x2, c2) = (%d, %d, %d, %d)" x1 c1 x2 c2)

-- | Estimate offset of description part from non-option lines
-- | Returns Just (offset size, match count) if matches
descOffsetWithCountSimple :: String -> Maybe (Int, Int)
descOffsetWithCountSimple s =
  assert ('\t' `notElem` s) res
  where
    locs = getNonoptLocations s
    optionOffset = getOptionOffset s
    cols = [x | (_, x) <- locs, Maybe.fromJust optionOffset + 3 <= x]
    res = getMostFrequentWithCount cols

-- | Estimate offset of description part from the lines with options
-- | Returns Just (offset size, match count) if matches
descOffsetWithCountInOptionLines :: String -> Maybe (Int, Int)
descOffsetWithCountInOptionLines s =
  assert ('\t' `notElem` s) res
  where
    sep = "   " -- Hardcode as 3 spaces for now
    optLines = filter startsWithDash (lines s)
    --
    -- reversed to handle spacing not multiples of 3
    -- for example `split "   " "--opt     desc"` gives ["--opt", "  desc"]`
    -- but I don't want spaces before "desc"
    xss = map (join sep . tail . split sep . reverse . rstrip) optLines
    res = getMostFrequentWithCount $ map ((n +) . length) $ filter (not . isSpacesOnly) xss
      where
        n = length sep


isSpacesOnly :: String -> Bool
isSpacesOnly = all (' ' ==)

isWordStartingAtOffsetAfterBlank :: Int -> String -> Bool
isWordStartingAtOffsetAfterBlank _ "" = False
isWordStartingAtOffsetAfterBlank n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    condBefore && condAfter
  where
    (before, after) = splitAt n x
    condBefore = isSpacesOnly before
    condAfter = head after /= ' '

isWordStartingAtOffset :: Int -> String -> Bool
isWordStartingAtOffset _ "" = False
isWordStartingAtOffset 0 x = head x /= ' '
isWordStartingAtOffset n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    last before == ' ' && head after /= ' '
  where
    (before, after) = splitAt n x

getOptionDescriptionPairsFromLayout :: String -> [(String, String)]
getOptionDescriptionPairsFromLayout content
  | Maybe.isNothing descriptionOffsetMay || Maybe.isNothing optionOffsetMay = []
  | otherwise = traceInfo $ concatMap (handleQuartet xs descOffset) quartets
  where
    s = convertTabsToSpaces 8 content
    xs = lines s
    optionOffsetMay = getOptionOffset s
    optOffset = Maybe.fromJust optionOffsetMay
    optLocsCandidates = getOptionLocations s
    (optLocs, optLocsExcluded) = List.partition (\(_, c) -> c == optOffset) optLocsCandidates
    optLineNums = map fst optLocs

    descriptionOffsetMay = getDescriptionOffset s
    descOffset = Maybe.fromJust descriptionOffsetMay

    descLineNums = [idx | (idx, x) <- zip [0 ..] xs, isWordStartingAtOffsetAfterBlank descOffset x]

    traceInfo = trace (
      printf
        (unlines (map ("[debug] " ++) ["option offset: %d", "description offset: %d", "optLocs: %s", "descLineNums: %s"]))
        optOffset descOffset (show optLocs) (show descLineNums))
    triples = toConsecutiveRangeTriples optLineNums descLineNums
    quartets = [(a, b, getDescFrom a b, c) | (a, b, c) <- triples] -- [(optFrom, optTo, descFrom descTo)]
    getDescFrom optFrom optTo
      | null ys = optTo
      | otherwise = descFrom
      where
        indices = [optTo - 1, optTo -2 .. optFrom]
        ys = takeWhile (\i -> isWordStartingAtOffset descOffset (xs !! i)) indices
        descFrom = last ys

handleQuartet :: [String] -> Int -> (Int, Int, Int, Int) -> [(String, String)]
handleQuartet xs descOffset (optFrom, optTo, descFrom, descTo)
  | optFrom == descFrom = asrt $ ss ++ [s2]
  | otherwise = [s1] ++ ss ++ [s2]
  where
    asrt = assert (optFrom <= descFrom && descFrom <= optTo && optTo <= descTo)
    s1 = squashOptions optFrom (descFrom + 1) xs descOffset
    ss = oneliners (descFrom + 1) (optTo - 1) xs descOffset
    s2 = squashDescriptions (optTo - 1) descTo xs descOffset

squashOptions :: Int -> Int -> [String] -> Int -> (String, String)
squashOptions a b xs offset = (opt, desc)
  where
    optLines = map (\i -> take offset (xs !! i)) [a, a + 1 .. b]
    opt = join "," optLines
    desc = drop offset (xs !! (b - 1))

squashDescriptions :: Int -> Int -> [String] -> Int -> (String, String)
squashDescriptions a b xs offset = (opt, desc)
  where
    descLines = map (\i -> drop offset (xs !! i)) [a, a + 1 .. b]
    opt = take offset (xs !! a)
    desc = unwords descLines

oneliners :: Int -> Int -> [String] -> Int -> [(String, String)]
oneliners a b xs offset =
  map (\i -> splitAt offset (xs !! i)) [a, a + 1 .. b]

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

-- |
-- [WARNING] O(N^2): rewrite if slow
toConsecutiveRangeTriples :: [Int] -> [Int] -> [(Int, Int, Int)]
toConsecutiveRangeTriples xs ys =
  [(x1, y1, y1) | (x1, x2) <- xRanges, (y1, y2) <- yRanges, x2 == y1]
  where
    xRanges = toRanges xs
    yRanges = toRanges ys

-- |  idxRange idxColFrom (inclusive) lines
--  extractRectangleToRight (2, 5) 3
--  ........
--  ........
--  ...xxxxx
--  ...xxxxx
--  ...xxxxx
--  ........
extractRectangleToRight :: (Int, Int) -> Int -> [String] -> String
extractRectangleToRight (rowFrom, rowTo) idxCol xs =
  unwords zs
  where
    ys = take (rowTo - rowFrom) (drop rowFrom xs)
    zs = map (drop idxCol) ys

ifTrace :: (a -> Bool) -> (a -> String) -> a -> a
ifTrace check exceptRun x
  | check x = trace (exceptRun x) x
  | otherwise = x
