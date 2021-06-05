{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import Control.Exception (assert)
import qualified Data.List as List
import Data.List.Extra (nubSort)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.String.Utils (join, rstrip, split, strip)
import Debug.Trace (trace)
import HelpParser (parseLine, parseWithOptPart, preprocessAllFallback)
import Text.Printf (printf)
import Type (Opt)
import Utils (convertTabsToSpaces, debugMsg, debugShow, getMostFrequent, getMostFrequentWithCount, getParagraph, smartUnwords, startsWithDash, toRanges)

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
_getOffsetHelper getLocs s = traceMessage res
  where
    locs = getLocs s
    xs = map snd locs
    res = getMostFrequent xs
    droppedOptionLinesInfo = unlines [(printf "[Dropped] %03d: %s" r (lines s !! r) :: String) | (r, c) <- locs, Just c /= res]
    traceMessage = trace droppedOptionLinesInfo

-- | get presumed horizontal offsets of options lines
-- Here the number is plural as the short options and the long options
-- can appear with different justifications (i.e. docker --help)
getOptionOffsets :: String -> [Int]
getOptionOffsets s = case (short, long) of
  (Nothing, Nothing) -> []
  (Nothing, Just y) -> [y]
  (Just x, Nothing) -> [x]
  (Just x, Just y) -> if x == y then [x] else [x, y]
  where
    long = getLongOptionOffset s
    short = getShortOptionOffset s

----------------------------------------
-- For 3-pane layout (short-option   long-option   description)

-- | check if the string starts with -- possibly after spaces and tabs
startsWithDoubleDash :: String -> Bool
startsWithDoubleDash s = case ss of
  "" -> False
  [_] -> False
  c1 : c2 : _ -> c1 == '-' && c2 == '-'
  where
    ss = dropWhile (`elem` " \t") s

startsWithSingleDash :: String -> Bool
startsWithSingleDash s = case ss of
  "" -> False
  [_] -> False
  c1 : c2 : _ -> c1 == '-' && c2 /= '-'
  where
    ss = dropWhile (`elem` " \t") s

-- | get location of long options
getLongOptionLocations :: String -> [Location]
getLongOptionLocations = _getNonblankLocationTemplate startsWithDoubleDash

-- | get location of long options
getShortOptionLocations :: String -> [Location]
getShortOptionLocations = _getNonblankLocationTemplate startsWithSingleDash

-- | get presumed horizontal offset of long options
getLongOptionOffset :: String -> Maybe Int
getLongOptionOffset = _getOffsetHelper getLongOptionLocations

-- | get presumed horizontal offset of long options
getShortOptionOffset :: String -> Maybe Int
getShortOptionOffset = _getOffsetHelper getShortOptionLocations

----------------------------------------

-- There are two independent ways to guess the horizontal offset of descriptions
--   1) A description line may be simply offset by space
--   2) A description line may appear following an option
--      	... from the pattern that the description and the options+args
--      	may be separated by 3 or more spaces
-- Returns Nothing if 1 and 2 disagrees, or no information in 1 and 2
--
getDescriptionOffset :: String -> [Int] -> Int -> Maybe Int
getDescriptionOffset s optLineNums optOffset =
  case (descOffsetWithCountSimple s optLineNums optOffset, descOffsetWithCountInOptionLines s optLineNums) of
    (Nothing, Nothing) -> trace "[WARNING] Retrieved absolutely zero information" Nothing
    (Nothing, q) -> trace "Descriptions always appear in the lines with options" $ fmap fst q
    (p, Nothing) -> trace "Descriptions never appear in the lines with options" $ fmap fst p
    (Just (x1, c1), Just (x2, c2))
      | x1 == x2 -> Just x1
      | c1 <= 3 && 3 < c2 -> debug Just x2
      | c2 <= 3 && 3 < c1 -> debug Just x1
      | 0 < x1 - x2 && x1 - x2 < 5 -> debug (Just x2) -- sometimes continued lines are indented.
      | otherwise -> debug Nothing
      where
        msg =
          "[WARNING] Disagreement in offsets:\n\
          \   description-only-line offset   %d (with count %d)\n\
          \   option+description-line offset %d (with count %d)\n"
        debug = trace (printf msg x1 c1 x2 c2 :: String)

-- | Estimate offset of description part from non-option lines
-- | Returns Just (offset size, match count) if matches
descOffsetWithCountSimple :: String -> [Int] -> Int -> Maybe (Int, Int)
descOffsetWithCountSimple s optLineNums optOffset =
  assert ('\t' `notElem` s) res
  where
    locs = getNonoptLocations s
    cols =
      [ x | (r, x) <- locs,
            -- short option followed by a single space = 3
            optOffset + 3 <= x,
            -- description can exist only around option lines
            head optLineNums < r, r < last optLineNums + 5
            -- previous line cannot be blank
      ]
    res = getMostFrequentWithCount cols

-- | Estimate offset of description part from the lines with options
-- | Returns Just (offset size, match count) if matches
descOffsetWithCountInOptionLines :: String -> [Int] -> Maybe (Int, Int)
descOffsetWithCountInOptionLines s optLineNums =
  assert ('\t' `notElem` s) res
  where
    sep = "   " -- Hardcode as 3 spaces for now
    xs = lines s

    -- reversed to handle spacing not multiples of 3
    -- for example `split sep "--opt     desc"` == ["--opt", "  desc"]`
    -- but I don't want spaces at the beginning of the description
    xss = map (join sep . tail . split sep . reverse . rstrip . (xs !!)) optLineNums
    res = getMostFrequentWithCount $ map ((n +) . length) $ filter (not . isSpacesOnly) xss
      where
        n = length sep

isSpacesOnly :: String -> Bool
isSpacesOnly s = not (null s) && all (' ' ==) s

isWordStartingAtOffsetAfterBlank :: Int -> String -> Bool
isWordStartingAtOffsetAfterBlank _ "" = False
isWordStartingAtOffsetAfterBlank n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    condBefore && condAfter
  where
    (before, after) = splitAt n x
    condBefore = isSpacesOnly before
    condAfter = not (null after) && head after /= ' '

isWordStartingAtOffset :: Int -> String -> Bool
isWordStartingAtOffset _ "" = False
isWordStartingAtOffset 0 (c : _) = c /= ' '
isWordStartingAtOffset n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    not (null before) && not (null after) && last before == ' ' && head after /= ' '
  where
    (before, after) = splitAt n x

isWordStartingAround :: Int -> Int -> String -> Bool
isWordStartingAround _ _ "" = False
isWordStartingAround margin idx x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    any criteria indices
  where
    indices = [idx - margin, idx - margin + 1 .. idx + margin]
    criteria i =
      not (null before) && not (null after) && last before == ' ' && head after /= ' '
      where
        (before, after) = splitAt i x

isSeparatedAtOffset :: Int -> String -> String -> Bool
isSeparatedAtOffset _ _ "" = False
isSeparatedAtOffset n sep x
  | n <= w || length x <= w = False
  | otherwise =
    assert ('\n' `notElem` x && '\t' `notElem` x) $
      sep `List.isSuffixOf` before && (not (null after) && head after /= ' ')
  where
    w = length sep
    (before, after) = splitAt n x

-- ================================================
-- ============== Main stuff ======================

-- | Returns option-description pairs based on layouts AND also returns the dropped
-- line index ranges that is uncaught in the process.
getOptionDescriptionPairsFromLayout :: String -> Maybe ([(String, String)], [(Int, Int)])
getOptionDescriptionPairsFromLayout content
  | null optionOffsets || Maybe.isNothing descriptionOffsetMay = Nothing
  | length optLineNums <= 3 = Nothing
  | otherwise = Just $ traceInfo (res, dropped)
  where
    s = convertTabsToSpaces 8 content
    xs = lines s
    optionOffsets = debugMsg "Option offsets:" $ getOptionOffsets s
    optLocsCandidates = getOptionLocations s
    (optLocs, optLocsExcluded) = List.partition (\(_, c) -> c `elem` optionOffsets) optLocsCandidates
    optLineNums = debugShow "optLocsExcluded:" optLocsExcluded $ debugMsg "optLineNums" $ map fst optLocs
    optLineNumsSet = Set.fromList optLineNums

    descriptionOffsetMay = getDescriptionOffset s optLineNums (List.maximum optionOffsets)
    offset = debugMsg "Description offset:" $ Maybe.fromJust descriptionOffsetMay

    -- More accomodating description line matching seems to work better...
    descLineNumsWithoutOption = [idx | (idx, x) <- zip [0 ..] xs, isWordStartingAtOffsetAfterBlank offset x]
    linewidths = map (length . (xs !!)) descLineNumsWithoutOption
    descriptionLineWidthMax = debugMsg "descriptionLineMax" $ if null linewidths then 80 else List.maximum linewidths
    descLineNumsWithoutOptionSet = Set.fromList descLineNumsWithoutOption

    -- The line must be long when description starts at the option line and continues to the next line.
    -- Here I mean "long" by the
    isOptionAndDescriptionLine idx
      | not (isOptionLine idx) = False
      | otherwise =
        (not (isOptionLine (idx + 1)) && not (isDescriptionOnly (idx + 1)))
          || (isDescriptionOnly (idx + 1) && (length (xs !! idx) + 5 > descriptionLineWidthMax))
          || (not . null . parseLine $ (xs !! idx))
          || isOptionAndDescriptionLine (idx + 1)
      where
        isOptionLine i = i `Set.member` optLineNumsSet
        isDescriptionOnly i = i `Set.member` descLineNumsWithoutOptionSet

    descLineNumsWithOption =
      [ idx | idx <- optLineNums, isWordStartingAround 2 offset (xs !! idx), isOptionAndDescriptionLine idx
      ]
    descLineNums = debugMsg "descLineNums" $ nubSort (descLineNumsWithoutOption ++ descLineNumsWithOption)

    (quartets, dropped) = debugMsg "quartets" $ toConsecutiveRangeQuartets optLineNums descLineNums
    quartetsMod = debugMsg "quartetsMod" $ [(a, b, updateDescFrom xs offset a c, d) | (a, b, c, d) <- quartets] -- [(optFrom, optTo, descFrom, descTo)]
    res = concatMap (handleQuartet xs offset) quartetsMod
    traceInfo = trace ("[debug] Dropped option indices " ++ show dropped)

handleQuartet :: [String] -> Int -> (Int, Int, Int, Int) -> [(String, String)]
handleQuartet xs offset (optFrom, optTo, descFrom, descTo)
  | optFrom == descFrom && optTo == descTo = debug $ onelinersF optFrom optTo
  | optFrom == descFrom = debug $ onelinersF optFrom (optTo - 1) ++ [squashDescSideF (optTo - 1) descTo]
  | optTo == descFrom = debug [squashDescTopF descFrom descTo]
  | optTo == descTo = debug $ squashOptsF optFrom (descFrom + 1) : onelinersF (descFrom + 1) descTo
  | otherwise = debug $ (s1 : ss) ++ [s2]
  where
    debug = trace (show (optFrom, optTo, descFrom, descTo) ++ " ")
    squashOptsF = squashOptions xs offset
    squashDescSideF = squashDescriptionsSide xs offset
    squashDescTopF = squashDescriptionsTop xs offset
    onelinersF = oneliners xs offset
    s1 = squashOptsF optFrom (descFrom + 1)
    ss = onelinersF (descFrom + 1) (optTo - 1)
    s2 = squashDescSideF (optTo - 1) descTo

squashOptions :: [String] -> Int -> Int -> Int -> (String, String)
squashOptions xs offset a b = trace "[squashOptions] " (opt, desc)
  where
    optLines = map (xs !!) $ take (b - a) [a, a + 1 ..]
    optLinesLastTruncated = map strip (init optLines ++ [take offset (last optLines)])
    opt = join "," optLinesLastTruncated
    desc = drop offset (xs !! (b - 1))

squashDescriptionsSide :: [String] -> Int -> Int -> Int -> (String, String)
squashDescriptionsSide xs offset a b = trace "[squashDescriptionsSide] " (opt, desc)
  where
    descLines = map (drop offset . (xs !!)) $ take (b - a) [a, a + 1 ..]
    opt = strip $ take offset (xs !! a)
    desc = smartUnwords descLines

squashDescriptionsTop :: [String] -> Int -> Int -> Int -> (String, String)
squashDescriptionsTop xs offset a b = trace "[squashDescriptionsTop] " (opt, desc)
  where
    descLines = map (drop offset . (xs !!)) $ take (b - a) [a, a + 1 ..]
    opt = strip (xs !! (a - 1))
    desc = smartUnwords descLines

oneliners :: [String] -> Int -> Int -> Int -> [(String, String)]
oneliners xs offset a b =
  [ trace "[oneliners] " (strip former, latter)
    | i <- take (b - a) [a, a + 1 ..],
      let (former, latter) = splitAt offset (xs !! i)
  ]

updateDescFrom :: [String] -> Int -> Int -> Int -> Int
updateDescFrom xs offset optFrom descFrom
  | null ys = descFrom
  | otherwise = debugMsg "updateDescFrom (res) =" res
  where
    indices = take (optFrom - descFrom) [descFrom - 1, descFrom - 2 ..]
    ys = takeWhile (\i -> isWordStartingAround 2 offset (xs !! i)) indices
    res = last ys

-- | Returns (optFrom, optTo, descFrom, descTo) quartets AND dropped indices xs
toConsecutiveRangeQuartets :: [Int] -> [Int] -> ([(Int, Int, Int, Int)], [(Int, Int)])
toConsecutiveRangeQuartets xs ys =
  (res, dropped)
  where
    (xRanges, yRanges) = makeRanges xs ys
    res = mergeRangesFast xRanges yRanges
    xsRes = Set.fromList [(x1, x2) | (x1, x2, _, _) <- res]
    dropped = filter (`Set.notMember` xsRes) xRanges

-- | Make pairs of ranges such that
-- when two ranges (x1, x2) and (y1, y2) overlaps,
-- they always satisfy   x1 <= y1 <= x2 <= y2
-- As a special case, x2 == y1 is considered as "overlap"
-- although [x1, x2) and [y1, y2) have empty intersection.
makeRanges :: [Int] -> [Int] -> ([(Int, Int)], [(Int, Int)])
makeRanges xs ys =
  (xRanges, yRanges)
  where
    xStarts = map fst (toRanges xs)
    yEnds = map snd (toRanges ys)
    (xssHead, xss) = List.mapAccumR f xs yEnds
    f acc y = span (< y) acc
    xRanges = concatMap toRanges (xssHead : if null xss then [] else init xss)
    (_, yss) = List.mapAccumR g ys xStarts
    g acc x = span (< x) acc
    yRanges = concatMap toRanges yss

-- | [deprecated] O(N^2) so replaced with mergeRangesFast
mergeRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
mergeRanges xs ys = [(x1, x2, y1, y2) | (x1, x2) <- xs, (y1, y2) <- ys, x1 <= y1 && y1 <= x2 && x2 <= y2]

-- | Create quartets (x1, x2, y1, y2) as overlapping boundaries
-- [Note] As a special case, x2 == y1 is considered as a overlap
-- although [x1, x2) and [y1, y2) have empty intersection.
mergeRangesFast :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
mergeRangesFast _ [] = []
mergeRangesFast [] _ = []
mergeRangesFast ((x1, x2) : xs) ((y1, y2) : ys)
  | x2 < y1 = mergeRangesFast xs ((y1, y2) : ys)
  | y2 <= x1 = mergeRangesFast ((x1, x2) : xs) ys
  | otherwise = assert cond $ (x1, x2, y1, y2) : mergeRangesFast xs ys
  where
    cond = x1 <= y1 && y1 <= x2 && x2 <= y2

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

preprocessAll :: String -> [(String, String)]
preprocessAll content = res
  where
    xs = lines content
    may = getOptionDescriptionPairsFromLayout content
    res = case may of
      Just (layoutResults, droppedIdxRanges) ->
        layoutResults ++ fallbackResults
        where
          paragraphs = map (getParagraph xs) droppedIdxRanges
          fallbackResults = debugMsg "opt-desc pairs from the fallback\n" $ concatMap preprocessAllFallback paragraphs
      Nothing ->
        trace "[warning] ignore layout" $ preprocessAllFallback content

parseMany :: String -> [Opt]
parseMany "" = []
parseMany s = List.nub . concat $ results
  where
    pairs = preprocessAll s
    results =
      [ (\xs -> if null xs then debugShow "[warning] Failed pair:" (optStr, descStr) xs else xs) $
          parseWithOptPart optStr descStr
        | (optStr, descStr) <- pairs,
          (optStr, descStr) /= ("", "")
      ]
