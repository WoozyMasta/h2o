{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module GenJSON where

import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Type (Command (..), Opt, Subcommand (..))

toSimpleCommand :: String -> String -> [Opt] -> Command
toSimpleCommand name desc opts = Command name desc opts []

subcommandToCommand :: Subcommand -> [Opt] -> Command
subcommandToCommand (Subcommand subcmd desc) = toSimpleCommand subcmd desc

toCommand :: String -> String -> [Opt] -> [(Subcommand, [Opt])] -> Command
toCommand name desc opts subcmdOptsPairs =
  Command name desc opts subcommands
  where
    subcommands = [subcommandToCommand subcmd opts' | (subcmd, opts') <- subcmdOptsPairs]

toJSONText :: Command -> Text
toJSONText = TL.toStrict . encodeToLazyText
