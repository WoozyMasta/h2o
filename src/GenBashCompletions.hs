{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenBashCompletions where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Type (Opt (..), OptName (..))

bashTemplate :: String
bashTemplate =
  unlines
    [ "# autogenerated bash script",
      "",
      "_func()",
      "{",
      "    local cur prev words cword",
      "    _init_completion -n = || return",
      "    opts=\"%s\"",
      "",
      "    if [[ ${cur} == -* ]] || (( COMP_CWORD == 1 )); then",
      "        COMPREPLY=( $(compgen -W \"${opts}\" -- \"${cur}\") )",
      "        return 0",
      "    fi",
      "",
      "    COMPREPLY=( $(compgen -W \"${opts}\" -- \"${cur}\") )",
      "    return 0",
      "}",
      "",
      "## -o bashdefault and -o default are fallback",
      "complete -F _func -o bashdefault -o default %s"
    ]

getOptsArray :: [Opt] -> Text
getOptsArray opts = T.unwords $ concatMap (map (T.pack . _raw) . _names) opts

genBashScript :: String -> [Opt] -> Text
genBashScript cmd opts = res
  where
    optsArray = getOptsArray opts
    res = T.pack $ printf bashTemplate optsArray cmd
