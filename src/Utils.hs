-- | get statistical mode (= the most frequently appeareing item)
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Utils where

import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.Map as Map


getMostFrequent :: (Ord a) => [a] -> a
getMostFrequent xs = x
  where
    counter = Map.toList $ Map.fromListWith (+) (map (,1) xs)
    (x, maxCount) = Foldable.maximumBy (compare `on` snd) counter
