module HelpParser where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

data Option = Option
  { _optionName :: OptionName,
    _optionDesc :: String,
    _optionArg :: Maybe String
  }

data OptionName = OptionName
  { _optionNameBase :: String,
    _optionNameType :: OptionNameType
  }

data OptionNameType = LONG | SHORT | OLD deriving (Eq, Show)

allDigits = "0123456789"

allAlphabets = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanum = allAlphabets ++ allDigits

dash :: ReadP Char
dash = satisfy (== '-')

ascii :: ReadP Char
ascii = satisfy $ \c -> c `elem` alphanum

singleSpace :: ReadP Char
singleSpace = char ' '

isAllowedCharInName :: Char -> Bool
isAllowedCharInName c = c `elem` ('-' : '_' : alphanum)

newline :: ReadP Char
newline = char '\n'

word :: ReadP String
word = munch1 (`notElem` " \t\n")

sentence :: ReadP String
sentence = do
  xs <- sepBy word (many1 singleSpace)
  return (unwords xs)

optionBase :: ReadP String
optionBase = do
  head <- ascii
  tail <- munch isAllowedCharInName
  return (head : tail)

longOptionName :: ReadP String
longOptionName = do
  _ <- count 2 dash
  name <- optionBase
  return ("--" ++ name)

shortOptionName :: ReadP String
shortOptionName = do
  _ <- dash
  c <- ascii
  return ('-' : c : "")

oldOptionName :: ReadP String
oldOptionName = do
  _ <- dash
  name <- optionBase
  return ('-' : name)

optionArg :: ReadP String
optionArg = do
  singleSpace <|> char '='
  arg <- many1 ascii
  singleSpace
  return arg

optionItem :: ReadP (String, String, Maybe String)
optionItem = do
  skipSpaces
  name <- longOptionName <|> (oldOptionName <++ shortOptionName)
  arg <- fmap Just optionArg <++ pure Nothing
  skipSpaces
  char ':' <++ pure 'x' -- pure 'x' is just a placeholder that always succeeds
  skipSpaces
  desc <- sentence
  skipSpaces
  skipMany1 newline <|> eof
  return (name, desc, arg)
