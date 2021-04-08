{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Main where

import Criterion.Main
import Text.FuzzyFind
import Control.DeepSeq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

deriving instance NFData Alignment
deriving instance NFData Result
deriving instance NFData ResultSegment

main :: IO ()
main = defaultMain
  [ env lipsumFile $ \lipsum ->
      bgroup "fzf"
        $   (\n -> bench (show (4 ^ n)) . nf (fuzzyFind ["lipsum"]) $ take
              (4 ^ n)
              lipsum
            )
        <$> [0 .. 6]
  ]

lipsumFile :: IO [Text]
lipsumFile =
  ((Text.take 50 <$>) . Text.split (\n -> n == '\n')) <$> Text.readFile "tests/lipsum.txt"

