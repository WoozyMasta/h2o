{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Postprocess
  ( fixCommand,
    fixShortOptWithArgWithoutSpace,
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
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
    relevantOptsScoreList = [zip opts scores | (opts, scores) <- zip relevantOptsList scoresList]
    msgHeader =
      [ "======================",
        "Duplicates of option names!",
        "============================="
      ]
    opnameOptsPairs = zip optNamesOfConcern relevantOptsScoreList
    pairsSorted = List.sortOn (\(name, xs) -> (- length xs, name)) opnameOptsPairs
    pairsStrList =
      concatMap
        (\(OptName raw _, xs) -> delim raw : ("  " ++ raw) : delim raw : (map showPair xs))
        pairsSorted
      where
        delim s = replicate (4 + length s) '='
    showPair :: (Opt, Int) -> String
    showPair (opt, score) = show score ++ "\n" ++ show opt
    msg = unlines (msgHeader ++ pairsStrList)
    warnTrace = Utils.warnShow msg ""

    isJustGood :: [OptName] -> Bool
    isJustGood nx = all (`notElem` optNamesOfConcern) nx
    hasShortAndLong :: [OptNameType] -> Bool
    hasShortAndLong ts = ShortType `elem` ts && LongType `elem` ts && OldType `notElem` ts
    optsFixed =
      [ opt | opt@(Opt names _ _) <- opts, let types = map _type names, isJustGood names || hasShortAndLong types
      ]

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
      | length (T.words t) == 1 = -3
      | Char.isLower (T.head t) = -1
      | (T.length . head . T.split (== '.')) t > 80 = -1
      | otherwise = 1

    argScore :: Text -> Int
    argScore text
      | T.null text = 1
      | Utils.isBracketed text = 2
      | Utils.hasMatchingBrackets text = 2
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
