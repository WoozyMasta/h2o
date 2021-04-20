{-# LANGUAGE DuplicateRecordFields #-}

module HelpParser where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
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

argWordBare :: ReadP String
argWordBare = do
  head <- satisfy (\c -> c `elem` alphanum ++ "({")
  tail <- munch1 (\c -> c `elem` (alphanum ++ "+-|{}"))
  return (head : tail)

argWordAngleBracketed :: ReadP String
argWordAngleBracketed = do
  (consumed, _) <- gather $ between (char '<') (char '>') nonBracketLettersForSure
  return consumed
  where
    nonBracketLettersForSure = munch1 (`notElem` "<>\n")

argWord :: ReadP String
argWord = argWordBare +++ argWordAngleBracketed

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
  let res = OptName "--" DoubleDashAlone
  singleSpace <++ pure 'x'  -- dummy 'x'
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
  singleSpace +++ char '='
  args <- sepBy1 argWord argSep
  return (List.intercalate "," args)

skip :: ReadP a -> ReadP ()
skip a = a >> return ()

-- very heuristic handling in separating description part
heuristicSep :: String -> ReadP String
heuristicSep args =
  f ":" <++ f ";" <++ f "\n" <++ string spaces
  where
    f s = optional singleSpace *> string s
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

argSep :: ReadP String
argSep = string ","

surroundedBySquareBracket :: ReadP String
surroundedBySquareBracket = do
  between (char '[') (char ']') nonBracketLettersForSure
  where
    nonBracketLettersForSure = munch1 (`notElem` "[]\n")

failWithBracket :: ReadP String
failWithBracket = unwords <$> sepBy1 w singleSpace
  where
    w = munch1 (`notElem` " []\n\t;:")

discardSquareBracket :: ReadP String
discardSquareBracket = do
  first <- optionNonBracket
  surroundedBySquareBracket
  second <- optionNonBracket
  return (first ++ second)
  where
    optionNonBracket = option "" failWithBracket

unwrapSquareBracket :: ReadP String
unwrapSquareBracket = do
  first <- optionNonBracket
  content <- surroundedBySquareBracket
  second <- optionNonBracket
  return (first ++ content ++ second)
  where
    optionNonBracket = option "" failWithBracket

squareBracketHandler :: ReadP String
squareBracketHandler = choice [failWithBracket, discardSquareBracket, unwrapSquareBracket]

-- Extract (optionPart, description) matches
preprocessor :: ReadP (String, String)
preprocessor = do
  skipSpaces
  (consumed, opt) <- gather squareBracketHandler
  heuristicSep consumed
  skipSpaces
  string ":" <++ string ";" <++ pure "x" -- always succeeds; consume the former if possible
  char '\n' <++ pure 'x'
  skipSpaces
  ss <- sepBy word singleSpace
  skipSpaces
  skip newline <++ eof
  let desc = unwords ss
  return (opt, desc)

-- takes description as external info
-- the first option argument ARG1 is extracted when you have a case like
--    "-o ARG1, --out=ARG2"
optPart :: String -> ReadP Opt
optPart desc = do
  pairs <- sepBy1 optNameArgPair optSep
  let names = map fst pairs
  let args = case filter (not . null) (map snd pairs) of
        [] -> ""
        xs -> head xs
  eof
  return (Opt names args desc)

parse :: String -> [Opt]
parse s = concat results
  where
    -- thanks to lazy evaluation, desc is NOT evaluated when xs == []
    -- so don't worry about calling (head xs).
    xs = readP_to_S preprocessor s
    candidates = map (fst . fst) xs
    results = map (map fst . readP_to_S (optPart desc)) candidates
    desc = snd . fst . head $ xs
