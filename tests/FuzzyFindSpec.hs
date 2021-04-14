module FuzzyFindSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Text.FuzzyFind
import Data.Sequence()

spec :: Spec
spec = do
  describe "FuzzyFind" $ do
    modifyMaxSuccess (const 1000)
      $ prop "Alignment contains original"
      $ \q d -> case bestMatch q d of
          Nothing                       -> True
          Just (Alignment _ (Result r)) -> foldMap segmentToString r == d

