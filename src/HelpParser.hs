{-# LANGUAGE DuplicateRecordFields #-}

module HelpParser where

import Control.Applicative ((<|>))
import Data.List as List
import Text.ParserCombinators.ReadP

data Opt = Opt
  { _names :: [OptName],
    _arg :: String,
    _desc :: String
  }
  deriving (Eq)

data OptName = OptName
  { _raw :: String,
    _type :: OptNameType
  }
  deriving (Eq)

instance Show Opt where
  show (Opt names args desc) =
    show (names, args, desc)

instance Show OptName where
  show (OptName raw t) = show raw

instance Ord OptName where
  (OptName raw1 t1) `compare` (OptName raw2 t2) = (raw1, t1) `compare` (raw2, t2)

instance Ord Opt where
  Opt n1 a1 d1 `compare` Opt n2 a2 d2 = (n1, a1, d1) `compare` (n2, a2, d2)

data OptNameType = LongType | ShortType | OldType | DoubleDashAlone deriving (Eq, Show, Ord)

allDigits = "0123456789"

allAlphabets = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanum = allAlphabets ++ allDigits

extraSymbols = "+-_!?"

dash :: ReadP Char
dash = satisfy (== '-')

ascii :: ReadP Char
ascii = satisfy $ \c -> c `elem` alphanum

singleSpace :: ReadP Char
singleSpace = char ' '

isAscii :: Char -> Bool
isAscii c = c `elem` alphanum

isAllowedOptChar :: Char -> Bool
isAllowedOptChar c = c `elem` (alphanum ++ extraSymbols)

newline :: ReadP Char
newline = char '\n'

word :: ReadP String
word = munch1 (`notElem` " \t\n")

argWord :: ReadP String
argWord = do
  head <- satisfy (\c -> c `elem` alphanum ++ "({<")
  tail <- munch1 (\c -> c `elem` (alphanum ++ "+-|<>{}"))
  return (head : tail)

description :: ReadP String
description = do
  xs <- sepBy1 word (munch1 (== ' '))
  return (unwords xs)

optWord :: ReadP String
optWord = do
  head <- ascii
  tail <- munch isAllowedOptChar
  return (head : tail)

longOptName :: ReadP OptName
longOptName = do
  _ <- count 2 dash
  name <- optWord
  let res = OptName ("--" ++ name) LongType
  return res

doubleDash :: ReadP OptName
doubleDash = do
  _ <- count 2 dash
  singleSpace
  let res = OptName "--" DoubleDashAlone
  return res

shortOptName :: ReadP OptName
shortOptName = do
  _ <- dash
  c <- ascii
  let res = OptName ('-' : c : "") ShortType
  return res

oldOptName :: ReadP OptName
oldOptName = do
  _ <- dash
  head <- ascii
  tail <- optWord
  let res = OptName ('-' : head : tail) OldType
  return res

optName :: ReadP OptName
optName = longOptName <++ doubleDash <++ oldOptName <++ shortOptName

optArgs :: ReadP String
optArgs = do
  singleSpace <|> char '='
  args <- sepBy1 argWord singleSpace
  return (List.intercalate "," args)

skip :: ReadP a -> ReadP ()
skip a = a >> return ()

-- very heuristic handling of description separator
-- because a wild case like following exists...
--   "  -O INT[,INT] gap open penalty [4,24]"
heuristicSep :: String -> ReadP String
heuristicSep args =
  f ":" <++ f ";" <++ f "\n" <++ string spaces
  where
    f s = optional singleSpace >> string s
    spaces = case args of
      "" -> twoSpaces
      args -> if last args `elem` ">}])" then oneSpace else twoSpaces
    twoSpaces = "  "
    oneSpace = " "

optNameArgPair :: ReadP (OptName, String)
optNameArgPair = do
  name <- optName
  args <- optArgs <++ pure ""
  return (name, args)

optSep :: ReadP String
optSep = sep +++ string " "
  where
    sep = do
      s <- string ","
      optional (string " ")
      return s

optItem :: ReadP Opt
optItem = do
  skipSpaces
  pairs <- sepBy1 optNameArgPair optSep
  let names = map fst pairs
  let args = case filter (not . null) (map snd pairs) of
        [] -> ""
        xs -> head xs
  heuristicSep args -- [FIXME] hate this
  skipSpaces
  string ":" <++ string ";" <++ pure "x" -- always succeeds; consume the former if possible
  char '\n' <++ pure 'x'
  skipSpaces
  desc <- description
  skipSpaces
  skip newline <++ eof
  return (Opt names args desc)
