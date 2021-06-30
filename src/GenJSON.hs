{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module GenJSON where

import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Type (Command (..))

toJSONText :: Command -> Text
toJSONText = TL.toStrict . encodeToLazyText
