{-# LANGUAGE DuplicateRecordFields #-}

module HelpParser where

import qualified Data.List as List
import Data.List.Extra (splitOn)
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP
import Type
  ( Opt (..),
    OptName (..),
    OptNameType (..),
  )

type OptArg = String

digitChars :: [Char]
digitChars = "0123456789"

alphChars :: [Char]
alphChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanumChars :: [Char]
alphanumChars = alphChars ++ digitChars

dash :: ReadP Char
dash = satisfy (== '-')

alphanum :: ReadP Char
alphanum = satisfy $ \c -> c `elem` alphanumChars

singleSpace :: ReadP Char
singleSpace = char ' '

isAlphanum :: Char -> Bool
isAlphanum c = c `elem` alphanumChars

isAllowedOptChar :: Char -> Bool
isAllowedOptChar c = c `elem` (alphanumChars ++ "+-_!?@.")

newline :: ReadP Char
newline = char '\n'

word :: ReadP String
word = munch1 (`notElem` " \t\n")

argWordBare :: ReadP String
argWordBare = do
  x <- satisfy (\c -> c `elem` alphanumChars ++ "\"`'_^(#.[")
  xs <- munch (\c -> c `elem` (alphanumChars ++ "\"`'_:<>)+-*/|#.=[]"))
  return (x : xs)

argWordBracketedHelper :: Char -> Char -> ReadP String
argWordBracketedHelper bra ket = do
  (consumed, _) <- gather $ between (char bra) (char ket) nonBracketLettersForSure
  lookedAhead <- look
  let focus = head . splitOn "  " . takeWhile (/= '\n') $ lookedAhead
  let beforeKet = takeWhile (/= ket) focus
  extended <-
    if (ket `List.elem` focus) && not (null beforeKet) && ('-' `List.notElem` beforeKet) && not (init consumed `List.isSuffixOf` beforeKet)
      then endingWithKet
      else pure ""
  return (consumed ++ extended)
  where
    nonBracketLettersForSure = munch1 (`notElem` ['\n', ket])
    endingWithKet = do
      chunk <- nonBracketLettersForSure
      end <- string [ket]
      return (chunk ++ end)

argWordBracketed :: ReadP String
argWordBracketed = argWordAngleBracketed <++ argWordCurlyBracketed <++ argWordParenthesized <++ argWordSquareBracketed <++ argWordDoubleQuoted <++ argWordSingleQuoted

argWordAngleBracketed :: ReadP String
argWordAngleBracketed = argWordBracketedHelper '<' '>'

argWordCurlyBracketed :: ReadP String
argWordCurlyBracketed = argWordBracketedHelper '{' '}'

argWordParenthesized :: ReadP String
argWordParenthesized = argWordBracketedHelper '(' ')'

argWordSquareBracketed :: ReadP String
argWordSquareBracketed = argWordBracketedHelper '[' ']'

argWordDoubleQuoted :: ReadP String
argWordDoubleQuoted = argWordBracketedHelper '"' '"'

argWordSingleQuoted :: ReadP String
argWordSingleQuoted = argWordBracketedHelper '\'' '\''

description :: ReadP String
description = do
  xs <- sepBy1 word (munch1 (== ' '))
  return (unwords xs)

optWord :: ReadP String
optWord = do
  x <- alphanum
  xs <- munch isAllowedOptChar
  -- For example docker run --help has "--docker*"
  _ <- char '*' <++ pure '*'
  return (x : xs)

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
  _ <- singleSpace <++ pure 'x' -- dummy 'x'
  return res

singleDash :: ReadP OptName
singleDash = do
  _ <- char '-'
  _ <- singleSpace <++ pure 'x'
  return $ OptName "-" SingleDashAlone

shortOptName :: ReadP OptName
shortOptName = do
  _ <- dash
  c <- alphanum +++ satisfy (`elem` "@$=?&#%")
  let res = OptName ('-' : c : "") ShortType
  return res

oldOptName :: ReadP OptName
oldOptName = do
  _ <- dash
  x <- alphanum
  xs <- optWord
  let res = OptName ('-' : x : xs) OldType
  return res

optName :: ReadP OptName
optName = longOptName <++ doubleDash <++ oldOptName <++ shortOptName <++ singleDash

optArgs :: ReadP String
optArgs = do
  _ <- char '=' <++ singleSpace <++ pure ' '
  _ <- munch (== ' ')
  args <- sepBy1 argWordBare argSep
  return (List.intercalate "," args)

optArgsInBraket :: ReadP String
optArgsInBraket = do
  _ <- char '=' <++ singleSpace <++ pure ' ' -- ok not to have a delimiter before
  _ <- munch (== ' ')
  (s, _) <- gather $ sepBy1 argWordBracketed (char ',' +++ char ' ' +++ pure ' ')
  return s

skip :: ReadP a -> ReadP ()
skip a = a *> pure ()

-- very heuristic handling in separating description part
heuristicSep :: String -> ReadP String
heuristicSep args =
  f ":\n" <++ f "\n" <++ f ": " <++ f "\t" <++ twoOrMoreSpaces <++ varSpaces
  where
    f s = optional singleSpace *> string s
    twoOrMoreSpaces = string " " *> munch1 (== ' ')
    varSpaces
      | null args = twoSpaces
      | last args `elem` ">}])" = oneSpace
      | otherwise = twoSpaces
    twoSpaces = string "  "
    oneSpace = string " "

optNameArgPair :: ReadP (OptName, String)
optNameArgPair = do
  name <- optName
  args <- sepBy (optArgsInBraket <++ optArgs) (char ' ')
  _ <- string "..." <++ pure ""
  return (name, unwords args)

optSep :: ReadP String
optSep = sep <++ munch1 (== ' ') <++ altsep
  where
    sep = do
      s <- string ","
      _ <- munch (== ' ')
      return s
    altsep = do
      _ <- munch (== ' ')
      s <- string "/"
      _ <- munch (== ' ')
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

unwrapSquareBracket :: ReadP String
unwrapSquareBracket = do
  first <- beforeSquareBraket
  content <- surroundedBySquareBracket
  second <- afterSquareBraket
  return (first ++ content ++ second)

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
  _ <- heuristicSep consumed -- this is the separator between optionPart and description
  skipSpaces
  desc <- munch1 ('\n' /=)
  skip newline <++ eof
  return (opt, desc)

fallback :: ReadP (String, String)
fallback = do
  _ <- munch ('\n' /=)
  skip newline
  return ("", "")

-- takes description as external info
-- the first option argument ARG1 is extracted when you have a case like
--    "-o ARG1, --out=ARG2"
optPart :: ReadP ([OptName], OptArg)
optPart = do
  skipSpaces
  pairs <- sepBy1 optNameArgPair optSep
  let names = map fst pairs
  let args = case filter (not . null) (map snd pairs) of
        [] -> ""
        x : _ -> x
  skipSpaces
  _ <- argWordParenthesized <++ pure " "
  skipSpaces
  eof
  return (names, args)

parseLine :: String -> [Opt]
parseLine s = List.nub . concat $ results
  where
    -- thanks to lazy evaluation, desc is NOT evaluated when xs == []
    -- so don't worry about calling (head xs).
    xs = readP_to_S preprocessor s
    pairs = map fst xs
    results =
      [ (\ys -> if null ys then trace ("Failed pair: " ++ show (optStr, descStr)) ys else ys) $
          map ((\(a, b) -> Opt a b descStr) . fst) $
            readP_to_S optPart optStr
        | (optStr, descStr) <- pairs
      ]

-- | Parse when options+args and description are given separately
-- Use optPart directly, but go back to preprocessor when
-- the parse fails. Failure happens mostly when the option name
-- contains [] such as `--[no-]copy`.
parseWithOptPart :: String -> String -> [Opt]
parseWithOptPart optStr descStr
  | (not . null) res = map ((\(a, b) -> Opt a b descStr) . fst) res
  | otherwise = trace "[warn] optPart fallback" $ parseLine (optStr ++ "   " ++ descStr) -- fallback
  where
    res = readP_to_S optPart optStr

preprocessAllFallback :: String -> [(String, String)]
preprocessAllFallback "" = []
preprocessAllFallback s = case readP_to_S (preprocessor <++ fallback) s of
  [] -> []
  (pair, rest) : moreMatches -> (pair : map fst moreMatches) ++ preprocessAllFallback rest
