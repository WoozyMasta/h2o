{-# LANGUAGE DuplicateRecordFields #-}

module HelpParser where

import qualified Data.List as List
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP
import Utils (debugMsg)

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

digitChars = "0123456789"

alphChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanumChars :: [Char]
alphanumChars = alphChars ++ digitChars

extraSymbolChars = "+-_!?@."

dash :: ReadP Char
dash = satisfy (== '-')

alphanum :: ReadP Char
alphanum = satisfy $ \c -> c `elem` alphanumChars

singleSpace :: ReadP Char
singleSpace = char ' '

isAlphanum :: Char -> Bool
isAlphanum c = c `elem` alphanumChars

isAllowedOptChar :: Char -> Bool
isAllowedOptChar c = c `elem` (alphanumChars ++ extraSymbolChars)

newline :: ReadP Char
newline = char '\n'

word :: ReadP String
word = munch1 (`notElem` " \t\n")

argWordBare :: ReadP String
argWordBare = do
  head <- satisfy (\c -> c `elem` alphanumChars ++ "\"'_^(#.[")
  tail <- munch (\c -> c `elem` (alphanumChars ++ "\"'_:<>)+-*/|#.=[]"))
  return (head : tail)

argWordBracketedHelper :: Char -> Char -> ReadP String
argWordBracketedHelper bra ket = do
  (consumed, _) <- gather $ between (char bra) (char ket) nonBracketLettersForSure
  return consumed
  where
    nonBracketLettersForSure = munch1 (`notElem` ['\n', bra, ket])

argWordAngleBracketed :: ReadP String
argWordAngleBracketed = argWordBracketedHelper '<' '>'

argWordCurlyBracketed :: ReadP String
argWordCurlyBracketed = argWordBracketedHelper '{' '}'

argWordParenthesized :: ReadP String
argWordParenthesized = argWordBracketedHelper '(' ')'

argWord :: ReadP String
argWord = argWordBare

description :: ReadP String
description = do
  xs <- sepBy1 word (munch1 (== ' '))
  return (unwords xs)

optWord :: ReadP String
optWord = do
  head <- alphanum
  tail <- munch isAllowedOptChar
  -- For example docker run --help has "--docker*"
  _ <- char '*' <++ pure '*'
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
  singleSpace <++ pure 'x' -- dummy 'x'
  return res

shortOptName :: ReadP OptName
shortOptName = do
  _ <- dash
  c <- alphanum +++ satisfy (`elem` "@$=?&#%")
  let res = OptName ('-' : c : "") ShortType
  return res

oldOptName :: ReadP OptName
oldOptName = do
  _ <- dash
  head <- alphanum
  tail <- optWord
  let res = OptName ('-' : head : tail) OldType
  return res

optName :: ReadP OptName
optName = longOptName <++ doubleDash <++ oldOptName <++ shortOptName

optArgs :: ReadP String
optArgs = do
  _ <- char '[' <++ pure ' ' -- for cases such as --cs[=STR]
  singleSpace +++ char '='
  args <- sepBy1 argWord argSep
  return (List.intercalate "," args)

optArgsInBraket :: ReadP String
optArgsInBraket = do
  char '=' <++ singleSpace <++ pure ' ' -- ok not to have a delimiter before
  args <- sepBy1 (argWordCurlyBracketed <++ argWordAngleBracketed <++ argWordParenthesized) (char ',' +++ char ' ') -- to keep { and }
  return (List.intercalate "," args)

skip :: ReadP a -> ReadP ()
skip a = a *> pure ()

-- very heuristic handling in separating description part
heuristicSep :: String -> ReadP String
heuristicSep args =
  f ":\n" <++ f "\n" <++ f ": " <++ f "\t" <++ twoOrMoreSpaces <++ varSpaces
  where
    f s = optional singleSpace *> string s
    twoOrMoreSpaces = string " " *> munch1 (== ' ')
    varSpaces = case args of
      "" -> twoSpaces
      args -> if last args `elem` ">}])" then oneSpace else twoSpaces
    twoSpaces = string "  "
    oneSpace = string " "

optNameArgPair :: ReadP (OptName, String)
optNameArgPair = do
  name <- optName
  args <- optArgsInBraket <++ optArgs <++ pure ""
  return (name, args)

optSep :: ReadP String
optSep = (sep +++ string " ") <++ string "/"
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
failWithBracket = do
  (s, _) <- gather (sepBy1 w (singleSpace +++ char ':'))
  return s
  where
    w = munch1 (`notElem` " :[]\n\t")

beforeSquareBraket :: ReadP String
beforeSquareBraket = do
  s <- option "" failWithBracket
  space <- option "" (string " ")
  rest <- look
  case rest of
    "" -> pure ()
    '[' : _ -> pure ()
    _ -> pfail
  return (s ++ space)

afterSquareBraket :: ReadP String
afterSquareBraket = do
  space <- option "" (string " ")
  s <- option "" failWithBracket
  return (space ++ s)

discardSquareBracket :: ReadP String
discardSquareBracket = do
  first <- beforeSquareBraket
  _ <- surroundedBySquareBracket
  second <- afterSquareBraket
  return (first ++ second)
  where
    optionNonBracket = option "" failWithBracket

unwrapSquareBracket :: ReadP String
unwrapSquareBracket = do
  first <- beforeSquareBraket
  content <- surroundedBySquareBracket
  second <- afterSquareBraket
  return (first ++ content ++ second)
  where
    optionNonBracket = option "" failWithBracket

squareBracketHandler :: ReadP String
squareBracketHandler = do
  x <- dash -- let if fail if not starting with '-'
  xs <- choice [failWithBracket, discardSquareBracket, unwrapSquareBracket]
  return (x : xs)

-- Extract (optionPart, description) matches
preprocessor :: ReadP (String, String)
preprocessor = do
  skipSpaces
  (consumed, opt) <- gather squareBracketHandler
  heuristicSep consumed -- this is the separator between optionPart and description
  skipSpaces
  desc <- munch1 ('\n' /=)
  skip newline <++ eof
  return (opt, desc)

fallback :: ReadP (String, String)
fallback = do
  line <- munch ('\n' /=)
  skip newline
  return ("", "")

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
  skipSpaces
  eof
  return (Opt names args desc)

parse :: String -> [Opt]
parse s = List.nub . concat $ results
  where
    -- thanks to lazy evaluation, desc is NOT evaluated when xs == []
    -- so don't worry about calling (head xs).
    xs = readP_to_S preprocessor s
    pairs = map fst xs
    results = [(\xs -> if null xs then trace ("Failed pair: " ++ show (optStr, descStr)) xs else xs) $ map fst $ readP_to_S (optPart descStr) optStr | (optStr, descStr) <- pairs]

preprocessAllFallback :: String -> [(String, String)]
preprocessAllFallback "" = []
preprocessAllFallback s = case readP_to_S (preprocessor <++ fallback) s of
  [] -> []
  (pair, rest) : moreMatches -> (pair : map fst moreMatches) ++ preprocessAllFallback rest
