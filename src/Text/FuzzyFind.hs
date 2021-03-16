{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Text.FuzzyFind where

import Control.Monad (join)
import Data.Array
  ( Array,
    (!),
  )
import qualified Data.Array as Array
import Data.Char (isAlphaNum, isLower, isUpper, toLower)
import Data.Foldable (maximumBy, toList, foldl')
import Data.Function (on)
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

data ResultSegment = Gap (Seq Char) | Match (Seq Char)
  deriving (Eq, Ord, Show, Generic)

newtype Result = Result { segments :: Seq ResultSegment }
  deriving (Eq, Ord, Show, Generic)

match :: Char -> Result
match a = Result [Match [a]]

gap :: Char -> Result
gap a = Result [Gap [a]]

reverseResult :: Result -> Result
reverseResult (Result xs) = Result . Seq.reverse $ reverseSegment <$> xs

reverseSegment :: ResultSegment -> ResultSegment
reverseSegment (Gap xs) = Gap (Seq.reverse xs)
reverseSegment (Match xs) = Match (Seq.reverse xs)

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
    Result [Gap (Seq.drop n g)] <> drop' (n - Seq.length g) (Result t)
  drop' n (Result (viewl -> Match g :< t)) =
    Result [Match (Seq.drop n g)] <> drop' (n - Seq.length g) (Result t)
  merge :: Result -> Result -> Result
  merge (Result Seq.Empty) ys                 = ys
  merge xs                 (Result Seq.Empty) = xs
  merge (Result xs)        (Result ys       ) = case (viewl xs, viewl ys) of
    (Gap g :< t, Gap g' :< t')
      | Seq.length g <= Seq.length g' -> Result [Gap g]
      <> merge (Result t) (drop' (Seq.length g) (Result ys))
      | otherwise -> Result [Gap g']
      <> merge (drop' (Seq.length g') (Result xs)) (Result t')
    (Match m :< t, Match m' :< t')
      | Seq.length m >= Seq.length m' -> Result [Match m]
      <> merge (Result t) (drop' (Seq.length m) (Result ys))
      | otherwise -> Result [Match m']
      <> merge (drop' (Seq.length m') (Result xs)) (Result t')
    (Gap g :< t, Match m' :< t') ->
      Result [Match m'] <> merge (drop' (Seq.length m') (Result xs)) (Result t')
    (Match m :< t, Gap g' :< t') ->
      Result [Match m] <> merge (Result t) (drop' (Seq.length m) (Result ys))

instance Semigroup Alignment where
  Alignment n r <> Alignment m s = Alignment (n + m) (mergeResults r s)

instance Monoid Alignment where
  mempty = Alignment 0 mempty

type Score = Int

data Alignment
  = Alignment { score :: Score, result :: Result }
  deriving (Eq, Ord, Show, Generic)

-- The base score given to a matching character
defaultMatchScore :: Int
defaultMatchScore = 16

-- The base score given to a mismatched character
defaultMismatchScore :: Int
defaultMismatchScore = 0

-- Bonus points given to characters matching at the beginning of words
defaultBoundaryBonus :: Int
defaultBoundaryBonus = defaultMatchScore `div` 2

-- Bonus points given to characters matching a hump of a CamelCase word.
-- We subtract a point from the word boundary score, since a word boundary will
-- incur a gap penalty.
defaultCamelCaseBonus :: Int
defaultCamelCaseBonus = defaultBoundaryBonus - 1

-- Double any bonus points for matching the first pattern of the character.
-- This way we strongly prefer starting the match at the beginning of a word.
defaultFirstCharBonusMultiplier :: Int
defaultFirstCharBonusMultiplier = 2

-- We prefer consecutive runs of matched characters in the pattern, so we
-- impose a penalty for any gaps, proportional to the size of the gap.
defaultGapPenalty :: Int -> Int
defaultGapPenalty 1 = 3
defaultGapPenalty n = max 0 (3 + n)

-- We give a bonus to consecutive matching characters.
-- A number about the same as the `boundaryBonus` will strongly prefer
-- runs of consecutive characters vs finding acronyms.
defaultConsecutiveBonus :: Int
defaultConsecutiveBonus = defaultGapPenalty 8

gaps :: String -> Result
gaps s = Result [Gap . Seq.fromList $ reverse s]

highlight :: Alignment -> String
highlight (Alignment s (Result segments)) =
  foldMap prettySegment segments <> "\n" <> foldMap showGaps segments
 where
  prettySegment (Gap   xs) = toList xs
  prettySegment (Match xs) = toList xs
  showGaps (Gap   xs) = replicate (length xs) ' '
  showGaps (Match xs) = replicate (length xs) '*'

bestMatch :: String -> String -> Maybe Alignment
bestMatch = bestMatch' defaultMatchScore
                       defaultMismatchScore
                       defaultGapPenalty
                       defaultBoundaryBonus
                       defaultCamelCaseBonus
                       defaultFirstCharBonusMultiplier
                       defaultConsecutiveBonus

fuzzyFind :: [String] -> [String] -> [Alignment]
fuzzyFind query strings =
  sortOn score
    $   strings
    >>= (\s -> toList
          $ foldl' (\a q -> (<>) <$> a <*> bestMatch q s) (Just mempty) query
        )

bestMatch'
  :: Int
  -> Int
  -> (Int -> Int)
  -> Int
  -> Int
  -> Int
  -> Int
  -> String
  -> String
  -> Maybe Alignment
bestMatch' matchScore mismatchScore gapPenalty boundaryBonus camelCaseBonus firstCharBonusMultiplier consecutiveBonus query str
  = Alignment (totalScore m nx) . reverseResult <$> traceback
 where
  totalScore i j = if i > m then 0 else hs ! (i, j) + bonuses ! (i, j)
  table = unlines
    [ unwords
      $ (if y > 0 then show $ b' ! y else "   ")
      : [ show (totalScore x y) | x <- [0 .. m] ]
    | y <- [0 .. n]
    ]
  similarity a b =
    if a == b || a == toLower b then matchScore else mismatchScore
  traceback = (gaps (drop nx str) <>) <$> go (m, nx)
  go (0, j) = pure $ gaps (take j str)
  go (i, 0) = Nothing
  go (i, j) = if similarity (a' ! i) (b' ! j) > 0
    then (match (b' ! j) <>) <$> go (i - 1, j - 1)
    else (gap (b' ! j) <>) <$> go (i, j - 1)
  nx = localMax m n
  localMax m n = maximumBy
    (\b d -> compare (totalScore m b) (totalScore m d))
    [ j | j <- [1 .. n] ]
  m       = length query
  n       = length str
  a'      = Array.listArray (1, m) query
  b'      = Array.listArray (1, n) str
  hs      = Array.listArray bounds [ h i j | (i, j) <- Array.range bounds ]
  bonuses = Array.listArray bounds [ bonus i j | (i, j) <- Array.range bounds ]
  bounds  = ((0, 0), (m, n))
  bonus 0 j = 0
  bonus i 0 = 0
  bonus i j = if similarity (a' ! i) (b' ! j) > 0
    then multiplier * (boundary + camel + consecutive)
    else 0
   where
    boundary =
      if j < 2 || isAlphaNum (b' ! j) && not (isAlphaNum (b' ! (j - 1)))
        then boundaryBonus
        else 0
    camel = if j > 1 && isLower (b' ! (j - 1)) && isUpper (b' ! j)
      then camelCaseBonus
      else 0
    multiplier = if i == 1 then firstCharBonusMultiplier else 1
    consecutive =
      let
        similar = i > 0 && j > 0 && similarity (a' ! i) (b' ! j) > 0
        afterMatch =
          i > 1 && j > 1 && similarity (a' ! (i - 1)) (b' ! (j - 1)) > 0
        beforeMatch =
          i < m && j < n && similarity (a' ! (i + 1)) (b' ! (j + 1)) > 0
      in
        if similar && (afterMatch || beforeMatch) then consecutiveBonus else 0
  h 0 _ = 0
  h _ 0 = 0
  h i j = scoreMatch `max` scoreGap `max` 0
   where
    scoreMatch =
      hs ! (i - 1, j - 1) + similarity (a' ! i) (b' ! j) + bonuses ! (i, j)
    scoreGap = maximum [ hs ! (i, j - l) - gapPenalty l | l <- [1 .. j] ]
