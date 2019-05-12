module Data.Relation.Internal
  ( Relation(..)
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

-- |
-- Representation of a relation on ordered (@Ord@) values
data Relation a b  = Relation
  { domain :: M.Map a (S.Set b)
  , range  :: M.Map b (S.Set a)
  } deriving (Show, Eq, Ord)
