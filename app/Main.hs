module Main where

import qualified Data.List as List
import HelpParser
import Subcommand
import System.Environment (getArgs)

main :: IO ()
main = do
  files <- getArgs
  content <- readFile (head files)
  let xs = parseMany content
  putStr $ List.intercalate "\n" $ map show xs


  -- [TODO] Introduce subcommand to switch?
  -- let xs = parseSubcommand content
  -- putStr $ List.intercalate "\n" $ map show xs