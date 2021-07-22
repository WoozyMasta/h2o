{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Postprocess
  ( fixCommand,
    fixShortOptWithArgWithoutSpace,
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.List.Extra (concatUnzip)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Type (Command (..), Opt (..), OptName (..), OptNameType (..))
import qualified Utils

fixOptName :: String -> OptName -> OptName
fixOptName arg_ (OptName raw t) =
  if arg_ `List.isSuffixOf` raw && t == OldType && length arg_ + 2 == length raw
    then Utils.debugMsg "altOptName" $ OptName (take 2 raw) ShortType
    else OptName raw t

-- | Fix short option followed by argument without space as disguised as an old option
--     example) For option names ["-Ttagsfile", "--tag-file"] with arg "tagsfile",
--              it's highly likely that the correct names are ["-T", "--tag-file"].
fixShortOptWithArgWithoutSpace :: Opt -> Opt
fixShortOptWithArgWithoutSpace (Opt names arg desc) = optFixed
  where
    namesFixed = map (fixOptName arg) names
    optFixed = Opt namesFixed arg desc

tallyDuplicates :: [Opt] -> [(OptName, Int)]
tallyDuplicates opts = Utils.count optnames
  where
    optnames = concatMap _names opts

fixDuplicateOpts :: [Opt] -> [Opt]
fixDuplicateOpts opts
  | null optNamesOfConcern = opts
  | otherwise = warnTrace optsFixed
  where
    -- [NOTE] inefficient for now
    pairs = [(optname, count) | (optname, count) <- tallyDuplicates opts, count > 1]
    (optNamesOfConcern, _) = unzip pairs
    relevantOptsList =
      map
        (\opname -> [opt | opt@(Opt names _ _) <- opts, opname `elem` names])
        optNamesOfConcern
    scoresList = map (map score) relevantOptsList
    relevantOptsScoreList = [zip opts_ scores_ | (opts_, scores_) <- zip relevantOptsList scoresList]
    msgHeader =
      [ "======================",
        "Duplicates of option names!",
        "============================="
      ]
    opnameOptsPairs = zip optNamesOfConcern relevantOptsScoreList
    pairsSorted = List.sortOn (\(name, xs) -> (- length xs, name)) opnameOptsPairs
    pairsStrList =
      concatMap
        (\(OptName raw _, xs) -> delim raw : ("  " ++ raw) : delim raw : map showPair xs)
        pairsSorted
      where
        delim s = replicate (4 + length s) '='

    (kept, discardedPairs) = concatUnzip (map keepOrDiscard relevantOptsScoreList)
    discarded = [opt_ | (opt_, score_) <- discardedPairs, score_ < 1, opt_ `notElem` kept]
    optsFixed =
      Utils.warnShow "Discarded opts due to duplicates: " discarded $
        filter (`notElem` discarded) opts

    keepOrDiscard :: [(a, Int)] -> ([a], [(a, Int)])
    keepOrDiscard itemsWithScore = (kept_, discardedPairs_)
      where
        (_, scores) = unzip itemsWithScore
        maxval = List.maximum scores
        (keptPairs, discardedPairs_) = List.partition (\(_, score_) -> score_ == maxval) itemsWithScore
        kept_ = map fst keptPairs

    showPair :: (Opt, Int) -> String
    showPair (opt_, score_) = show score_ ++ "\n" ++ show opt_
    msg = unlines (msgHeader ++ pairsStrList)
    warnTrace = Utils.warnShow msg ""

score :: Opt -> Int
score (Opt names arg desc) =
  sum
    [ descScore (T.pack desc),
      argScore (T.pack arg),
      nameScore names
    ]
  where
    descScore :: Text -> Int
    descScore t
      | T.null t = -10
      | length (T.words t) == 1 = -4
      | Char.isLower (T.head t) = -1
      | (T.length . head . T.split (== '.')) t > 80 = -1
      | T.last t == '.' = 1
      | otherwise = 0

    argScore :: Text -> Int
    argScore text
      | T.null text = 0
      | Utils.isBracketed text && Utils.hasMatchingBrackets text = 2
      | Utils.isBracketed text || Utils.hasMatchingBrackets text = 1
      | length (T.words text) == 1 && T.all Char.isUpper text = 2
      | length (T.words text) == 1 = 1
      | otherwise = -1

    nameScore :: [OptName] -> Int
    nameScore optnames
      | null optnames = error "Something is wrong with option names"
      | length optnames == 1 && topElem `elem` [ShortType, LongType, OldType] = 1
      | Set.fromList [LongType, ShortType] == types = 2
      | otherwise = 0
      where
        types = Set.fromList $ map _type optnames
        topElem = Set.elemAt 0 types

fixCommand :: Command -> Command
fixCommand (Command name desc opts subcmds) = Command name desc optsFixed subcmdsFixed
  where
    optsFixed = fixDuplicateOpts $ map fixShortOptWithArgWithoutSpace opts
    subcmdsFixed = map fixCommand subcmds
