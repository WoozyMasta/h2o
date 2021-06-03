{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text.IO as TIO
import Io (config, run)
import Options.Applicative
  ( execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    (<**>),
  )

main :: IO ()
main = execParser opts >>= run >>= TIO.putStr
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "Parse help or manpage texts, extract command options, and generate shell completion scripts"
        )
