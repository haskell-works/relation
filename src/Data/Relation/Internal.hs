module Data.Relation.Internal
  ( Relation(..)
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

-- |
-- This implementation avoids using @"S.Set (a,b)"@ because
-- it it is necessary to search for an item without knowing both @D@ and @R@.
--
-- In "S.Set", you must know both values to search.
--
-- Thus, we have are two maps to updated together.
--
-- 1. Always be careful with the associated set of the key.
--
-- 2. If you union two relations, apply union to the set of values.
--
-- 3. If you subtract, take care when handling the set of values.
--
-- As a multi-map, each key is asscoated with a Set of values v.
--
-- We do not allow the associations with the 'empty' Set.
data Relation a b  = Relation
  { domain ::  M.Map a (S.Set b)
  , range  ::  M.Map b (S.Set a)
  } deriving (Show, Eq, Ord)
