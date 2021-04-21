module Main where

import HelpParser
import System.Environment (getArgs)
import qualified Data.List as List


main :: IO ()
main = do
  files <- getArgs
  content <- readFile (head files)
  let xs = parseMany content
  putStr $ List.intercalate "\n" $ map show xs
