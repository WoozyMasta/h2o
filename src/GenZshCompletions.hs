{-# LANGUAGE DuplicateRecordFields #-}

module GenZshCompletions where

import qualified Data.List as List
import Data.String.Utils (replace)
import HelpParser
import Subcommand
import Text.Printf


zshHeader :: String -> String
zshHeader cmd = printf "#compdef %s\n\n" cmd :: String

getZshOptStr :: Opt -> String
getZshOptStr (Opt optnames _ desc) = case raws of
  [raw] -> printf "'%s[%s]'" raw quotedDesc :: String
  _ -> printf "'(%s)'{%s}'[%s]'" exclusionList optionNames quotedDesc :: String
  where
    raws = map _raw optnames
    exclusionList = unwords raws
    optionNames = List.intercalate "," raws
    quotedDesc = replace "]" "\\]" . replace "[" "\\[" . replace "'" "'\\''" $ desc

getZshDescStr :: Subcommand -> String
getZshDescStr (Subcommand name desc) = printf "'%s:%s'" name quotedDesc
  where
    quotedDesc = replace "'" "'\\''" desc

indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

genZshBodyOptions :: String -> [Opt] -> String
genZshBodyOptions _ opts = res
  where
    args = unlines (map (indent 4 . getZshOptStr) opts)
    containsOldStyle = elem OldType $ concatMap (map _type . _names) opts
    flags = if containsOldStyle then "" else "-s "
    template = "args=(\n%s)\n\n_arguments %s$args\n"
    res = printf template args flags :: String

genZshBodyCommands :: String -> [Subcommand] -> String
genZshBodyCommands cmd xs = res
  where
    args = unlines (map (indent 4 . getZshDescStr) xs)
    template = "cmds=(\n%s)\n\n_describe '%s commands' $cmds\n"
    res = printf template args cmd

genZshScript :: String -> [Opt] -> String
genZshScript cmd opts = header ++ body
  where
    header = zshHeader cmd
    body = genZshBodyOptions cmd opts
