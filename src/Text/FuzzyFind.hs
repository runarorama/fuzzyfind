{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : FuzzyFind
Description : Provides fuzzy matching on text
Copyright   : Unison Computing, 2021
License     : MIT
Maintainer  : runar.bjarnason@unison.cloud
Stability   : experimental

A package that provides an API for fuzzy text search in Haskell, using a
modified version of the Smith-Waterman algorithm. The search is intended to
behave similarly to the excellent fzf tool by Junegunn Choi.
-}
module Text.FuzzyFind where

import Control.Monad (join)
import Data.Massiv.Array
  ( Array,
    (!),
    Ix2(..),
    (...),
    forM,
    forM_
  )
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as M
import Data.Char (isAlphaNum, isLower, isUpper, toLower)
import Data.Foldable (maximumBy, toList, foldl')
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Sequence
  ( Seq (..),
    ViewL (..),
    ViewR (..),
    viewl,
    viewr,
    (<|)
  )
import Data.List (sortOn)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as Text

-- | @bestMatch query string@ will return 'Nothing' if @query@ is not a
-- subsequence of @string@. Otherwise, it will return the "best" way to line up
-- the characters in @query@ with the characters in @string@. Lower-case
-- characters in the @query@ are assumed to be case-insensitive, and upper-case
-- characters are assumed to be case-sensitive.
--
-- For example:
--
-- @
-- > bestMatch "ff" \"FuzzyFind\"
-- Just (Alignment {score = 25, result = Result {[Match \"F\", Gap "uzzy", Match \"F\", Gap "ind"]}})
-- @
--
-- The score indicates how "good" the match is. Better matches have higher
-- scores. There's no maximum score (except for the upper limit of the 'Int'
-- datatype), but the lowest score is @0@.
--
-- A substring from the query will generate a 'Match', and any characters from
-- the input that don't result in a 'Match' will generate a 'Gap'.
-- Concatenating all the 'Match' and 'Gap' results should yield the original
-- input string.
--
-- Note that the matched characters in the input always occur in the same order
-- as they do in the query pattern.
--
-- The algorithm prefers (and will generate higher scores for) the following
-- kinds of matches:
--
--   1. Contiguous characters from the query string. For example, @bestMatch "pp"@
-- will find the last two ps in "pickled pepper".
--   2. Characters at the beginnings of words. For example, @bestMatch "pp"@
-- will find the first two Ps in \"Peter Piper\".
--   3. Characters at CamelCase humps. For example, @bestMatch "bm" \"BatMan\"@
-- will score higher than @bestMatch "bm" \"Batman\".@
--   4. The algorithm strongly prefers the first character of the query pattern
-- to be at the beginning of a word or CamelHump. For example,
-- @bestMatch "mn" \"Bat Man\"@ will score higher than @bestMatch "atn" \"Batman\"@.
--
-- All else being equal, matches that occur later in the input string are preferred.
bestMatch :: String -- ^ The query pattern.
          -> String -- ^ The input string.
          -> Maybe Alignment
bestMatch = bestMatch' defaultMatchScore
                       defaultMismatchScore
                       defaultGapPenalty
                       defaultBoundaryBonus
                       defaultCamelCaseBonus
                       defaultFirstCharBonusMultiplier
                       defaultConsecutiveBonus

-- | Finds input strings that match all the given input patterns. For each input
-- that matches, it returns one 'Alignment'. The output is not sorted.
-- ascending.
--
-- For example:
--
-- @
-- > import Data.Foldable
-- > traverse_ (putStrLn . ("\\n" ++) . highlight) $ fuzzyFind ["dad", "mac", "dam"] ["red macadamia", "Madam Card"]
--
-- Madam Card
-- * *** ** *
--
-- red macadamia
--   * *******
-- @
fuzzyFind
  :: [String] -- ^ The query patterns.
  -> [String] -- ^ The input strings.
  -> [Alignment]
fuzzyFind = (fmap fst .) . fuzzyFindOn id

-- | A version of 'fuzzyFind' that searches on the given text field of the data.
fuzzyFindOn :: (a -> String) -> [String] -> [a] -> [(Alignment, a)]
fuzzyFindOn f query d =
  d
    >>= (\s ->
          toList
            $   (, s)
            <$> foldl' (\a q -> (<>) <$> a <*> bestMatch q (f s))
                       (Just mempty)
                       query
        )

instance Semigroup Alignment where
  Alignment n r <> Alignment m s = Alignment (n + m) (mergeResults r s)

instance Monoid Alignment where
  mempty = Alignment 0 mempty

type Score = Int

-- | An 'Alignment' is a 'Score' together with a 'Result'. Better results have
-- higher scores.
data Alignment
  = Alignment { score :: !Score, result :: !Result }
  deriving (Eq, Ord, Show, Generic)

-- | The base score given to a matching character
defaultMatchScore :: Int
defaultMatchScore = 16

-- | The base score given to a mismatched character
defaultMismatchScore :: Int
defaultMismatchScore = 0

-- | Bonus points given to characters matching at the beginning of words
defaultBoundaryBonus :: Int
defaultBoundaryBonus = defaultMatchScore `div` 2

-- | Bonus points given to characters matching a hump of a CamelCase word.
-- We subtract a point from the word boundary score, since a word boundary will
-- incur a gap penalty.
defaultCamelCaseBonus :: Int
defaultCamelCaseBonus = defaultBoundaryBonus - 1

-- | Double any bonus points for matching the first pattern of the character.
-- This way we strongly prefer starting the match at the beginning of a word.
defaultFirstCharBonusMultiplier :: Int
defaultFirstCharBonusMultiplier = 2

-- | We prefer consecutive runs of matched characters in the pattern, so we
-- impose a penalty for any gaps, which is added to the size of the gap.
defaultGapPenalty :: Int
defaultGapPenalty = 3

-- | We give a bonus to consecutive matching characters.
-- A number about the same as the `boundaryBonus` will strongly prefer
-- runs of consecutive characters vs finding acronyms.
defaultConsecutiveBonus :: Int
defaultConsecutiveBonus = 11

-- | Renders an 'Alignment' as a pair of lines with "*" on the lower line
-- indicating the location of pattern matches.
highlight' :: Alignment -> Text
highlight' (Alignment s (Result segments)) =
  foldMap prettySegment segments <> "\n" <> foldMap showGaps segments
 where
  prettySegment (Gap   xs) = xs
  prettySegment (Match xs) = xs
  showGaps (Gap   xs) = Text.pack $ replicate (Text.length xs) ' '
  showGaps (Match xs) = Text.pack $ replicate (Text.length xs) '*'

highlight :: Alignment -> String
highlight = Text.unpack . highlight'

-- | A highly configurable version of 'bestMatch'.
bestMatch'
  :: Int -- ^ Base score for a matching character. See 'defaultMatchScore'.
  -> Int -- ^ Base score for a mismatched character. See 'defaultMismatchScore'.
  -> Int -- ^ Additional penalty for a gap. See 'defaultGapPenalty'.
  -> Int -- ^ Bonus score for a match at the beginning of a word. See 'defaultBoundaryBonus'.
  -> Int -- ^ Bonus score for a match on a CamelCase hump. See 'defaultCamelCaseBonus'.
  -> Int -- ^ Bonus multiplier for matching the first character of the pattern.
         --   See 'defaultFirstCharBonusMultiplier'.
  -> Int -- ^ Bonus score for each consecutive character matched.
         --   See 'defaultFirstCharBonusMultiplier'.
  -> String -- ^ The query pattern.
  -> String -- ^ The input string.
  -> Maybe Alignment
bestMatch' matchScore mismatchScore gapPenalty boundaryBonus camelCaseBonus firstCharBonusMultiplier consecutiveBonus query str
  = Alignment (totalScore m nx) . reverseResult <$> traceback
 where
  totalScore i j =
    if i > m then 0 else (A.index' hs (i :. j)) + (A.index' bonuses (i :. j))
  -- table = unlines
  --   [ unwords
  --     $ (if y > 0 then show $ b' ! y else "   ")
  --     : [ show (totalScore x y) | x <- [0 .. m] ]
  --   | y <- [0 .. n]
  --   ]
  similarity a b =
    if a == b || a == toLower b then matchScore else mismatchScore
  traceback = go (gaps (drop nx str)) m nx
  go r 0 j = pure $ r <> gaps (take j str)
  go _ _ 0 = Nothing
  go r i j = if similarity (A.index' a' (i - 1)) (A.index' b' (j - 1)) > 0
    then go (r <> match (A.index' b' (j - 1))) (i - 1) (j - 1)
    else go (r <> gap (A.index' b' (j - 1))) i (j - 1)
  nx = localMax m n
  localMax m n = maximumBy
    (\b d -> compare (totalScore m b) (totalScore m d))
    [ j | j <- [1 .. n] ]
  a' = A.fromList A.Seq query :: Array A.P A.Ix1 Char
  b' = A.fromList A.Seq str :: Array A.P A.Ix1 Char
  m  = A.unSz $ A.size a'
  n  = A.unSz $ A.size b'
  hs :: Array A.P Ix2 Int
  hs = M.createArrayST_ (A.Sz (m + 1 :. n + 1)) $ \marr -> do
    A.forM_ ((0 :. 0) ... (m :. n)) $ \(i :. j) -> if (i == 0 || j == 0)
      then M.writeM marr (i :. j) 0
      else do
        scoreMatch <- do
          hprev <- M.readM marr ((i - 1) :. (j - 1))
          pure
            $ hprev
            + similarity (A.index' a' (i - 1)) (A.index' b' (j - 1))
            + A.index' bonuses (i :. j)
        scoreGap <- do
          (arr :: Array A.P A.Ix1 Int) <- forM (1 ... j) $ \l ->
            (\x -> x - (l + gapPenalty)) <$> M.readM marr (i :. (j - l))
          pure . fromMaybe 0 $ A.maximumM arr
        M.writeM marr (i :. j) (scoreMatch `max` scoreGap `max` 0)
  bonuses = A.makeArray A.Seq (A.Sz (m + 1 :. n + 1)) f :: Array A.P Ix2 Int
    where f (i :. j) = bonus i j
  bonus :: Int -> Int -> Int
  bonus 0 j = 0
  bonus i 0 = 0
  bonus i j = if similarity (A.index' a' (i - 1)) (A.index' b' (j - 1)) > 0
    then multiplier * (boundary + camel + consecutive)
    else 0
   where
    boundary =
      if j < 2 || isAlphaNum (A.index' b' (j - 1)) && not
           (isAlphaNum (A.index' b' (j - 2)))
        then boundaryBonus
        else 0
    camel =
      if j > 1 && isLower (A.index' b' (j - 2)) && isUpper (A.index' b' (j - 1))
        then camelCaseBonus
        else 0
    multiplier = if i == 1 then firstCharBonusMultiplier else 1
    consecutive =
      let
        similar =
          i
            >  0
            && j
            >  0
            && similarity (A.index' a' (i - 1)) (A.index' b' (j - 1))
            >  0
        afterMatch =
          i
            >  1
            && j
            >  1
            && similarity (A.index' a' (i - 2)) (A.index' b' (j - 2))
            >  0
        beforeMatch =
          i < m && j < n && similarity (A.index' a' i) (A.index' b' j) > 0
      in
        if similar && (afterMatch || beforeMatch) then consecutiveBonus else 0
  -- h 0 _ = 0
  -- h _ 0 = 0
  -- h i j = scoreMatch `max` scoreGap j 0
  --  where
  --   scoreMatch =
  --     (A.index' hs (i - 1 :. j - 1))
  --       + similarity (A.index' a' i) (A.index' b' j)
  --       + A.index' bonuses (i :. j)
  --   scoreGap l x = if l <= 1
  --     then x
  --     else
  --       let y = A.index' hs (i :. j - l) - (l + gapPenalty)
  --       in  scoreGap (l - 1) (max x y)

data ResultSegment = Gap !Text | Match !Text
  deriving (Eq, Ord, Show, Generic)

-- | Concatenating all the 'ResultSegment's should yield the original input string.
newtype Result = Result { segments :: Seq ResultSegment }
  deriving (Eq, Ord, Show, Generic)

match :: Char -> Result
match a = Result [Match $ Text.pack [a]]

gap :: Char -> Result
gap a = Result [Gap $ Text.pack [a]]

gaps :: String -> Result
gaps s = Result [Gap . Text.pack $ reverse s]

reverseResult :: Result -> Result
reverseResult (Result xs) = Result . Seq.reverse $ reverseSegment <$> xs

reverseSegment :: ResultSegment -> ResultSegment
reverseSegment (Gap xs) = Gap (Text.reverse xs)
reverseSegment (Match xs) = Match (Text.reverse xs)

instance Monoid Result where
  mempty = Result []

instance Semigroup Result where
  Result Empty <> as = as
  as <> Result Empty = as
  Result (viewr -> h :> Gap []) <> as = Result h <> as
  as <> Result (viewl -> Gap [] :< t) = as <> Result t
  Result (viewr -> h :> Match []) <> as = Result h <> as
  as <> Result (viewl -> Match [] :< t) = as <> Result t
  Result (viewr -> i :> Gap l) <> Result (viewl -> Gap h :< t) =
    Result (i <> [Gap (l <> h)] <> t)
  Result (viewr -> i :> Match l) <> Result (viewl -> Match h :< t) =
    Result (i <> [Match (l <> h)] <> t)
  Result a <> Result b = Result (a <> b)

mergeResults :: Result -> Result -> Result
mergeResults as bs = merge as bs
 where
  drop' :: Int -> Result -> Result
  drop' n m | n < 1 = m
  drop' n (Result (viewl -> Gap g :< t)) =
    Result [Gap (Text.drop n g)] <> drop' (n - Text.length g) (Result t)
  drop' n (Result (viewl -> Match g :< t)) =
    Result [Match (Text.drop n g)] <> drop' (n - Text.length g) (Result t)
  merge :: Result -> Result -> Result
  merge (Result Seq.Empty) ys                 = ys
  merge xs                 (Result Seq.Empty) = xs
  merge (Result xs)        (Result ys       ) = case (viewl xs, viewl ys) of
    (Gap g :< t, Gap g' :< t')
      | Text.length g <= Text.length g' -> Result [Gap g]
      <> merge (Result t) (drop' (Text.length g) (Result ys))
      | otherwise -> Result [Gap g']
      <> merge (drop' (Text.length g') (Result xs)) (Result t')
    (Match m :< t, Match m' :< t')
      | Text.length m >= Text.length m' -> Result [Match m]
      <> merge (Result t) (drop' (Text.length m) (Result ys))
      | otherwise -> Result [Match m']
      <> merge (drop' (Text.length m') (Result xs)) (Result t')
    (Gap g :< t, Match m' :< t') ->
      Result [Match m'] <> merge (drop' (Text.length m') (Result xs)) (Result t')
    (Match m :< t, Gap g' :< t') ->
      Result [Match m] <> merge (Result t) (drop' (Text.length m) (Result ys))
