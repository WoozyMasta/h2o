{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenZshCompletions where

import qualified Constants
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Formatting
import Text.Printf (printf)
import Type
  ( Command (..),
    Opt (..),
    OptName (..),
    OptNameType (..),
    Subcommand (..),
    asSubcommand,
  )

zshHeader :: String -> Text
zshHeader cmd = sformat ("#compdef _" % string % " " % string % "\n\n") cmd cmd

zshHeaderOld :: String -> Text
zshHeaderOld = sformat ("#compdef " % string % "\n\n")

quote :: Text -> Text
quote = T.replace "]" "\\]" . T.replace "[" "\\[" . T.replace "'" "'\\''"

getOptAsText :: Opt -> Text
getOptAsText (Opt optnames arg desc)
  | "file" `List.isInfixOf` arg || "path" `List.isInfixOf` arg = T.concat ["'", formatted, ":file:_files", "'"]
  | otherwise = T.concat ["'", formatted, "'"]
  where
    raws = map _raw optnames
    exclusionList = unwords raws
    optionNames = List.intercalate "," raws
    quotedDesc = quote . T.pack $ desc
    formatted = case raws of
      [raw] -> T.pack $ printf "%s[%s]" raw quotedDesc
      _ -> T.pack $ printf "(%s)'{%s}'[%s]" exclusionList optionNames quotedDesc

getSubcommandAsText :: Subcommand -> Text
getSubcommandAsText (Subcommand name desc) =
  T.pack $ printf "'%s:%s'" name quotedDesc
  where
    quotedDesc = quote (T.pack desc)

indent :: Int -> Text -> Text
indent n t = T.replicate n " " `T.append` t

addSuffix :: Text -> Text -> Text
addSuffix suffix line = line `T.append` suffix

genZshBodyOptions :: String -> [Opt] -> Text
genZshBodyOptions _ opts = res
  where
    args = T.unlines (map (indent 4 . getOptAsText) opts)
    containsOldStyle = elem OldType $ concatMap (map _type . _names) opts
    flags = if containsOldStyle then T.empty else "-s "
    template = "args=(\n%s)\n\n_arguments %s$args\n"
    res = T.pack $ printf template args flags

genZshBodyRootOptions :: String -> [Opt] -> Bool -> Text
genZshBodyRootOptions _ opts isSubcmdsNull =
  T.concat $ map T.unlines [linesPrefix, linesCore, linesSuffix]
  where
    linesPrefix =
      [ "",
        "    _arguments -C \\"
      ]
    linesSuffix
      | isSubcmdsNull =
        [ "        '1: :_commands' \\",
          "        '*: :_files'",
          ""
        ]
      | otherwise =
        [ "        ': :->cmd' \\",
          "        '*:: :->subcmd'",
          ""
        ]
    linesCore = map (addSuffix " \\" . indent 8 . getOptAsText) opts

genZshBodySubcommands :: [Subcommand] -> Text
genZshBodySubcommands subcommands = res
  where
    textPrefix =
      [ "    function _commands {",
        "        local -a commands",
        "        commands=("
      ]
    textCore = map (indent 12 . getSubcommandAsText) subcommands
    textSuffix =
      [ "        )",
        "        _describe 'command' commands",
        "    }",
        " "
      ]

    res = T.unlines $ concat [textPrefix, textCore, textSuffix]

zshSubcommandOptionFunction :: String -> Command -> Text
zshSubcommandOptionFunction name (Command subname _ opts _) =
  T.concat $ map T.unlines [linesPrefix, linesCore, linesSuffix]
  where
    linesPrefix =
      [ sformat ("    function _" % string % "_" % string % " {") name subname,
        "        _arguments \\"
      ]
    linesSuffix =
      [ "            \"*: :_files\"",
        "",
        "    }",
        ""
      ]
    linesCore = map (addSuffix " \\" . indent 12 . getOptAsText) opts

zshSubcommandOptionCall :: String -> String -> Text
zshSubcommandOptionCall name subname = T.unlines xs
  where
    xs =
      [ sformat ("        (" % string % ")") subname,
        sformat ("            _" % string % "_" % string) name subname,
        "            ;;",
        ""
      ]

genZshBodySubcommandOptions :: String -> [Command] -> Text
genZshBodySubcommandOptions cmd subcommands =
  T.concat [textPrefix, textCore, textSuffix]
  where
    subnames = [subname | (Command subname _ _ _) <- subcommands]
    textPrefix =
      T.unlines
        [ "    case $state in",
          "    (cmd)",
          "        _commands",
          "        ;;",
          "    (subcmd)",
          "        case $line[1] in"
        ]
    textCore = T.concat $ map (zshSubcommandOptionCall cmd) subnames
    textSuffix =
      T.unlines
        [ "        esac",
          "        ;;",
          "     esac",
          ""
        ]

meta :: Text
meta = "# Auto-generated with h2o\n\n"

genZshScript :: String -> [Opt] -> Text
genZshScript cmd opts = T.concat [header, meta, body]
  where
    header = zshHeaderOld cmd
    body = genZshBodyOptions cmd opts

toZshScript :: Command -> Text
toZshScript (Command name _ opts subcmds) =
  T.concat [textHeader, meta, textSubcmdFuncs, textFunctionOpening, textSubcommands, textRootOptions, textSubcommandOptionCalls, textFunctionClosing]
  where
    subcommands = map asSubcommand subcmds

    textHeader = zshHeader name
    textSubcmdFuncs = T.concat $ map (zshSubcommandOptionFunction name) subcmds
    textFunctionOpening =
      T.unlines
        [ "",
          sformat ("function _" % string % " {") name,
          "    local line state",
          ""
        ]
    textSubcommands = genZshBodySubcommands subcommands
    textRootOptions = genZshBodyRootOptions name opts (null subcmds)
    textSubcommandOptionCalls = genZshBodySubcommandOptions name subcmds
    textFunctionClosing = T.unlines ["}", ""]
