{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Criterion.Main
import Data.List
import Text.FuzzyFind
import Control.DeepSeq

deriving instance NFData Alignment
deriving instance NFData Result
deriving instance NFData ResultSegment

main :: IO ()
main = defaultMain
  [ env lipsumFile $ \lipsum -> bgroup
      "fzf"
      [ bench "1" . nf (fuzzyFind ["lipsum"]) $ take 1 lipsum
      , bench "5" . nf (fuzzyFind ["lipsum"]) $ take 5 lipsum
      , bench "10" . nf (fuzzyFind ["lipsum"]) $ take 10 lipsum
      , bench "50" . nf (fuzzyFind ["lipsum"]) $ take 100 lipsum
      ]
  ]

lipsumFile :: IO [String]
lipsumFile = lines <$> readFile "tests/lipsum.txt"

