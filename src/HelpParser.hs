{-# LANGUAGE DuplicateRecordFields #-}

module HelpParser where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

data Opt = Opt
  { _name :: OptName,
    _desc :: String,
    _arg :: Maybe String
  }

data OptName = OptName
  { _raw :: String,
    _type :: OptNameType
  }
  deriving (Eq)

instance Show OptName where
  show (OptName raw t) = show raw

data OptNameType = LongType | ShortType | OldType | DoubleDashAlone deriving (Eq, Show)

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
  head <- satisfy (\c -> c `elem` alphanum ++ "[({<")
  tail <- munch1 (\c -> c `elem` (alphanum ++ "+-[]<>{},"))
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
  let res = OptName "" DoubleDashAlone
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

altOptionName :: ReadP OptName
altOptionName = do
  char ','
  optional singleSpace
  optName

optNames :: ReadP [OptName]
optNames = do
  name <- optName
  maybeAlt <- fmap Just altOptionName <++ pure Nothing
  case maybeAlt of
    Just alt -> return [name, alt]
    Nothing -> return [name]

optArgs :: ReadP String
optArgs = do
  singleSpace <|> char '='
  args <- sepBy1 argWord singleSpace
  return (unwords args)

skip :: ReadP a -> ReadP ()
skip a = a >> return ()

-- very heuristic handling of description separator
-- because a wild case like following exists...
--   "  -O INT[,INT] gap open penalty [4,24]"
heuristicSep :: Maybe String -> ReadP String
heuristicSep maybeArgs =
  string ":" <++ string ";" <++ string args
  where
    args = case maybeArgs of
      Nothing -> "  "
      Just args -> if last args `elem` ">}])" then " " else "  "

optItem :: ReadP ([OptName], String, Maybe String)
optItem = do
  skipSpaces
  names <- optNames
  args <- fmap Just optArgs <++ pure Nothing
  heuristicSep args -- hate this
  skipSpaces
  string ":" <++ string ";" <++ pure "x" -- just a placeholder; consume the former if applies
  char '\n' <++ pure 'x'
  skipSpaces
  desc <- description
  skipSpaces
  skip newline <++ eof
  return (names, desc, args)
