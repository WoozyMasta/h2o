{-# LANGUAGE DuplicateRecordFields #-}

module GenBashCompletions where

import qualified Data.List as List
import HelpParser
import Text.Printf

type Command = String

bashTemplate :: String
bashTemplate =
  unlines
    [ "# autogenerated bash script",
      "_mycmd=%s",
      "",
      "_f()",
      "{",
      "    local cur prev words cword",
      "    _init_completion -n = || return",
      "    opts=\"%s\"",
      "",
      "    if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]]; then",
      "        COMPREPLY=( $(compgen -W \"${opts}\" -- \"${cur}\") )",
      "        return 0",
      "    fi",
      "",
      "    COMPREPLY=( $(compgen -W \"${opts}\" -- \"${cur}\") )",
      "    return 0",
      "}",
      "",
      "## -o bashdefault and -o default are fallback",
      "complete -F _f -o bashdefault -o default \"${_mycmd}\""
    ]

getOptsArray :: [Opt] -> String
getOptsArray opts = unwords $ concatMap (map _raw . _names) opts

indent n s = replicate n ' ' ++ s

genBashScript :: Command -> [Opt] -> String
genBashScript cmd opts = res
  where
    optsArray = getOptsArray opts
    res = printf bashTemplate cmd optsArray
