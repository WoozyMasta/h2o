{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Postprocess
  ( fixCommand,
    fixOpt,
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

fixOpt :: Opt -> Opt
fixOpt (Opt names arg desc) = optFixed
  where
    namesFixed = map (fixOptName arg) names
    optFixed = Opt namesFixed arg desc

-- | Convert old option into short option with argument if likely
--     example) For option names ["-Ttagsfile", "--tag-file"] with arg "tagsfile",
--              it's highly likely that the correct names are ["-T", "--tag-file"].
fixCommand :: Command -> Command
fixCommand (Command name desc opts subcmds) = Command name desc optsFixed subcmdsFixed
  where
    optsFixed = map fixOpt opts
    subcmdsFixed = map fixCommand subcmds
