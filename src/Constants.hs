{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.Text (Text)

-- [FIXME] Isn't there a good way to get it from package.yaml?
versionStr :: Text
versionStr = "0.1.7"
