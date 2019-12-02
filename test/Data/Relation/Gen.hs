module Data.Relation.Gen
  ( relation
  ) where

import Data.Relation
import Hedgehog
import Hedgehog.Range (Range)

import qualified Hedgehog.Gen as G

-- | Generate a relation given generators for the domain and range.
relation :: (MonadGen m, Ord a, Ord b) => Range Int -> m a -> m b -> m (Relation a b)
relation r ga gb = fmap fromList (G.list r ((,) <$> ga <*> gb))
