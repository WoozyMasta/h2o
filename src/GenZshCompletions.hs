{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenZshCompletions where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Type
  ( Opt (..),
    OptName (..),
    OptNameType (..),
    Subcommand (..),
  )

zshHeader :: String -> Text
zshHeader cmd = T.pack $ printf "#compdef %s\n\n" cmd

quote :: Text -> Text
quote = T.replace "]" "\\]" . T.replace "[" "\\[" . T.replace "'" "'\\''"

getOptAsText :: Opt -> Text
getOptAsText (Opt optnames _ desc) = case raws of
  [raw] -> T.pack $ printf "'%s[%s]'" raw quotedDesc
  _ -> T.pack $ printf "'(%s)'{%s}'[%s]'" exclusionList optionNames quotedDesc
  where
    raws = map _raw optnames
    exclusionList = unwords raws
    optionNames = List.intercalate "," raws
    quotedDesc = quote . T.pack $ desc

getSubcommandAsText :: Subcommand -> Text
getSubcommandAsText (Subcommand name desc) = T.pack $ printf "'%s:%s'" name quotedDesc
  where
    quotedDesc = quote (T.pack desc)

indent :: Int -> Text -> Text
indent n t = T.replicate n " " `T.append` t

genZshBodyOptions :: String -> [Opt] -> Text
genZshBodyOptions _ opts = res
  where
    args = T.unlines (map (indent 4 . getOptAsText) opts)
    containsOldStyle = elem OldType $ concatMap (map _type . _names) opts
    flags = if containsOldStyle then T.empty else "-s "
    template = "args=(\n%s)\n\n_arguments %s$args\n"
    res = T.pack $ printf template args flags

genZshBodyCommands :: String -> [Subcommand] -> Text
genZshBodyCommands cmd xs = res
  where
    args = T.unlines (map (indent 4 . getSubcommandAsText) xs)
    template = "cmds=(\n%s)\n\n_describe '%s commands' $cmds\n"
    res = T.pack $ printf template args cmd

genZshScript :: String -> [Opt] -> Text
genZshScript cmd opts = header `T.append` body
  where
    header = zshHeader cmd
    body = genZshBodyOptions cmd opts
