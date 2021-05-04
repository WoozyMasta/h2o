{-# LANGUAGE DuplicateRecordFields #-}

module GenZshCompletions where

import qualified Data.List as List
import HelpParser
import Subcommand
import Text.Printf

type Command = String

zshHeader :: Command -> String
zshHeader cmd = printf "#compdef %s\n\n" cmd :: String

getZshOptStr :: Opt -> String
getZshOptStr (Opt optnames args desc) = printf "'(%s)'{%s}'[%s]'" tag ops desc
  where
    raws = map _raw optnames
    tag = unwords raws
    ops = List.intercalate "," raws

getZshDescStr :: Subcommand -> String
getZshDescStr (Subcommand name desc) = printf "'%s:%s'" name desc

indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

genZshBodyOptions :: Command -> [Opt] -> String
genZshBodyOptions cmd opts = res
  where
    args = unlines (map (indent 4 . getZshOptStr) opts)
    containsOldStyle = elem OldType $ concatMap (map _type . _names) opts
    flags = if containsOldStyle then "" else "-s"
    template = "local -a args\nargs=(\n%s)\n\n_arguments %s args\n"
    res = printf template args flags :: String

genZshBodySubcommands :: Command -> [Subcommand] -> String
genZshBodySubcommands cmd xs = res
  where
    args = unlines (map (indent 4 . getZshDescStr) xs)
    template = "local -a subcommands\nsubcommands=(\n%s)\n\n_describe %s subcommands\n"
    res = printf template args

genZshScript :: Command -> [Opt] -> String
genZshScript cmd opts = header ++ body
  where
    header = zshHeader cmd
    body = genZshBodyOptions cmd opts
