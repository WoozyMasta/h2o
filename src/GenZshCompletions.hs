{-# LANGUAGE DuplicateRecordFields #-}

module GenZshCompletions where


import qualified Data.List as List
import HelpParser
import Text.Printf

type Command = String

getZshStr :: Opt -> String
getZshStr opt = printf "'(%s)'{%s}'[%s]'" tag opname desc
  where
    names = map _raw (_names opt)
    tag = unwords names
    opname = List.intercalate "," names
    desc = _desc opt

indent n s = replicate n ' ' ++ s

genZshScript :: Command -> [Opt] -> String
genZshScript cmd opts = res
  where
    args = unlines (map (indent 4 . getZshStr) opts)
    containsOldStyle = elem OldType $ concatMap (map _type . _names) opts
    flags = if containsOldStyle then "" else "-s"
    res = printf "#compdef %s\n\nlocal -a args\nargs=(\n%s)\n\n_arguments %s args\n" cmd args flags
