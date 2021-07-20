{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Postprocess
  ( fixCommand,
    fixShortOptWithArgWithoutSpace,
  )
where

import qualified Data.List as List
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
    pairs = [(optname, count) | (optname, count) <- tallyDuplicates opts, count > 1]
    (optNamesOfConcern, _) = unzip pairs
    msg =
      unlines
          [ "======================",
            "Duplicates of option names!",
            "============================="
          ]
    pairsSorted = List.sortOn (\(name, cnt) -> (-cnt, name)) pairs
    warnTrace = Utils.warnShow msg pairsSorted

    isJustGood :: [OptName] -> Bool
    isJustGood nx = all (`notElem` optNamesOfConcern) nx
    hasShortAndLong :: [OptNameType] -> Bool
    hasShortAndLong ts = ShortType `elem` ts && LongType `elem` ts && OldType `notElem` ts
    optsFixed =
      [ opt | opt@(Opt names _ _) <- opts, let types = map _type names, isJustGood names || hasShortAndLong types
      ]

fixCommand :: Command -> Command
fixCommand (Command name desc opts subcmds) = Command name desc optsFixed subcmdsFixed
  where
    optsFixed = fixDuplicateOpts $ map fixShortOptWithArgWithoutSpace opts
    subcmdsFixed = map fixCommand subcmds
