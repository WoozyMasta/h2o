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
  show (OptName raw t) = raw

data OptNameType = LongType | ShortType | OldType deriving (Eq, Show)

allDigits = "0123456789"

allAlphabets = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanum = allAlphabets ++ allDigits

extraSymbols = "+-_?"

dash :: ReadP Char
dash = satisfy (== '-')

ascii :: ReadP Char
ascii = satisfy $ \c -> c `elem` alphanum

singleSpace :: ReadP Char
singleSpace = char ' '

isAscii :: Char -> Bool
isAscii c = c `elem` alphanum

isAllowedCharInName :: Char -> Bool
isAllowedCharInName c = c `elem` (alphanum ++ extraSymbols)

newline :: ReadP Char
newline = char '\n'

word :: ReadP String
word = munch1 (`notElem` " \t\n")

argWord :: ReadP String
argWord = munch1 (\c -> c `elem` (alphanum ++ "[]<>{},"))

sentence :: ReadP String
sentence = do
  xs <- sepBy word (munch1 (== ' '))
  return (unwords xs)

optStem :: ReadP String
optStem = do
  head <- ascii
  tail <- munch isAllowedCharInName
  return (head : tail)

longOptName :: ReadP OptName
longOptName = do
  _ <- count 2 dash
  name <- optStem
  let res = OptName ("--" ++ name) LongType
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
  tail <- optStem
  let res = OptName ('-' : head : tail) OldType
  return res

optName :: ReadP OptName
optName = longOptName <|> (oldOptName <++ shortOptName)

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

optItem :: ReadP ([OptName], String, Maybe String)
optItem = do
  skipSpaces
  names <- optNames
  args <- fmap Just optArgs <++ pure Nothing
  skipSpaces
  char ':' <++ pure 'x' -- the latter is just a placeholder; consume the former if applies.
  char '\n' <++ pure 'x'
  skipSpaces
  desc <- sentence
  skipSpaces
  skip newline <++ eof
  return (names, desc, args)

