{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module GenJSON where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import HelpParser (Opt (..), OptName (..))
import Subcommand (Subcommand (..))

data Command = Command
  { name :: String, -- command name
    description :: String, -- description of command itself
    options :: [Opt], -- command options
    subcommands :: [Command] -- subcommands
  }
  deriving (Show)

instance ToJSON OptName where
  toJSON (OptName raw t) = toJSON raw
  toEncoding (OptName raw t) = toEncoding raw

instance ToJSON Opt where
  toJSON (Opt names arg desc) =
    object ["names" .= names, "argument" .= arg, "description" .= desc]

  toEncoding (Opt names arg desc) =
    pairs ("names" .= names <> "argument" .= arg <> "description" .= desc)

instance ToJSON Command where
  toJSON (Command name desc opts []) =
    object ["name" .= name, "description" .= desc, "options" .= opts]
  toJSON (Command name desc opts subcommands) =
    object ["name" .= name, "description" .= desc, "options" .= opts, "subcommands" .= subcommands]

  toEncoding (Command name desc opts []) =
    pairs ("name" .= name <> "description" .= desc <> "options" .= opts)
  toEncoding (Command name desc opts subcommands) =
    pairs ("name" .= name <> "description" .= desc <> "options" .= opts <> "subcommands" .= subcommands)

toSimpleCommand :: String -> String -> [Opt] -> Command
toSimpleCommand name desc opts = Command name name opts []

subcommandToCommand :: Subcommand -> [Opt] -> Command
subcommandToCommand (Subcommand subcmd desc) = toSimpleCommand subcmd desc

toCommand :: String -> String -> [Opt] -> [(Subcommand, [Opt])] -> Command
toCommand name desc opts subcmdOptsPairs =
  Command name desc opts subcommands
  where
    subcommands = [subcommandToCommand subcmd opts | (subcmd, opts) <- subcmdOptsPairs]

writeCommandAsJSON :: Command -> IO ()
writeCommandAsJSON = BL.putStr . encode
