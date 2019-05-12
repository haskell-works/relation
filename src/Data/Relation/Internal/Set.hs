module Data.Relation.Internal.Set
  ( flatten
  ) where

import Data.Set (Set)

import qualified Data.Set as S

-- |
-- Flatten a set of sets.
flatten :: Ord a => Set (Set a) -> Set a
flatten = S.foldr S.union S.empty
