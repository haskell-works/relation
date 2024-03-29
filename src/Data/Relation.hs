{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation
-- Copyright   :  (c) JK.  2019
--                (c) DD.  2012
--                (c) LFL. 2009
-- License     :  BSD-style
-- Maintainer  :  Drew Day<drewday@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Relations are modeled as assciations between two elements.
--
-- Relations offer efficient search for any of the two elements.
--
-- Unlike "Data.Map", an element ca be associated more than once.
--
-- The two purposes of this structure are:
--
-- 1. Associating elements
--
-- 2. Provide efficient searches for either of the two elements.
--
-- Since neither 'map' nor 'fold' are implemented, you /must/ convert
-- the structure to a list to process sequentially.
--
--
module Data.Relation (
    -- * The @Relation@ Type
    Relation

    -- *  Provided functionality:
    -- ** Questions
  , size            -- Number of Tuples in the relation?
  , null            -- Is empty?

    -- ** Construction
  , empty           -- Construct an empty relation.
  , fromList        -- Relation <- []
  , singleton       -- Construct a relation with a single element.

    -- ** Operations
  , union           -- Union of two relations.
  , unions          -- Union on a list of relations.
  , intersection    -- Intersection of two relations.
  , insert          -- Insert a tuple to the relation.
  , delete          -- Delete a tuple from the relation.
  , lookupDom       -- The Set of values associated with a value in the domain.
  , lookupRan       -- The Set of values associated with a value in the range.
  , memberDom       -- Is the element in the domain?
  , memberRan       -- Is the element in the range?
  , member          -- Is the tuple   in the relation?
  , notMember
  , restrictDom     -- Restrict the domain to that of the provided set
  , restrictRan     -- Restrict the range to that of the provided set
  , withoutDom      -- Restrict the domain to exclude elements of the provided set
  , withoutRan      -- Restrict the range to exclude elements of the provided set
  , (<-<)           -- Compose two relations
  , (>->)

    -- ** Conversion
  , toList          -- Construct a list from a relation
  , dom             -- Extract the elements of the range to a Set.
  , ran             -- Extract the elements of the domain to a Set.
  , converse        -- Converse of the relation
  ) where

import Control.Monad          (MonadPlus, guard)
import Data.Foldable          (fold)
import Data.Map               (Map)
import Data.Maybe             (fromMaybe)
import Data.Relation.Internal (Relation (Relation))
import Data.Set               (Set)
import Prelude                hiding (null)

import qualified Data.Foldable              as F
import qualified Data.Map                   as M
import qualified Data.Relation.Internal     as R
import qualified Data.Relation.Internal.Set as S
import qualified Data.Set                   as S

#if !MIN_VERSION_base(4,13,0)
import Data.Functor           (Functor ((<$)))
#endif

-- * Functions about relations

-- The size is calculated using the domain.
-- |  @size r@ returns the number of tuples in the relation.
-- >>> size (fromList []) == 0
-- True
-- >>> size (fromList [('a', 1)]) == 1
-- True
size :: Relation a b -> Int
size r = M.foldr ((+) . S.size) 0 (R.domain r)

-- | Construct a relation with no elements.
-- >>> toList (fromList []) == []
-- True
empty :: Relation a b
empty = Relation M.empty M.empty

-- |
-- The list must be formatted like: [(k1, v1), (k2, v2),..,(kn, vn)].
-- >>> toList (fromList [('a', 1)]) == [('a', 1)]
-- True
-- >>> fromList [('a', 1), ('a', 1)] == fromList [('a', 1), ('a', 1)]
-- True
-- >>> fromList [('a', 1), ('b', 1)] == fromList [('a', 1), ('b', 1)]
-- True
fromList :: (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList xs = Relation
  { R.domain  = M.fromListWith S.union $ snd2Set    xs
  , R.range   = M.fromListWith S.union $ flipAndSet xs
  }
  where snd2Set    = map (\(x, y) -> (x, S.singleton y))
        flipAndSet = map (\(x, y) -> (y, S.singleton x))

-- | Builds a List from a Relation.
-- >>> toList (fromList [('a', 1)]) == [('a', 1)]
-- True
-- >>> toList (fromList [('a', 1), ('b', 2)]) == [('a', 1), ('b', 2)]
-- True
-- >>> toList (fromList [('a', 1), ('a', 2)]) == [('a', 1), ('a', 2)]
-- True
-- >>> toList (fromList [('a', 1), ('a', 1)]) == [('a', 1)]
-- True
toList :: Relation a b -> [(a, b)]
toList r = concatMap (\(x, y) -> zip (repeat x) (S.toList y)) (M.toList . R.domain $ r)

-- |
-- Builds a 'Relation' consiting of an association between: @x@ and @y@.
-- >>> singleton 'a' 1 == fromList [('a', 1)]
-- True
singleton :: a -> b -> Relation a b
singleton x y  = Relation
  { R.domain  = M.singleton x (S.singleton y)
  , R.range   = M.singleton y (S.singleton x)
  }

-- | The 'Relation' that results from the union of two relations: @r@ and @s@.
-- >>> fromList [('a', 1)] `union` fromList [('a', 1)] == fromList [('a', 1)]
-- True
-- >>> fromList [('a', 2)] `union` fromList [('a', 2)] == fromList [('a', 2)]
-- True
-- >>> fromList [('a', 1)] `union` fromList [('b', 2)] == fromList [('a', 1), ('b', 2)]
-- True
union :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
union r s = Relation
  { R.domain  = M.unionWith S.union (R.domain r) (R.domain s)
  , R.range   = M.unionWith S.union (R.range  r) (R.range  s)
  }

-- | Union a list of relations using the 'empty' relation.
-- >>> unions [] == fromList []
-- True
-- >>> unions [fromList [('a', 1)]] == fromList [('a', 1)]
-- True
-- >>> unions [fromList [('a', 1)], fromList [('a', 1)]] == fromList [('a', 1)]
-- True
-- >>> unions [fromList [('a', 2)], fromList [('a', 2)]] == fromList [('a', 2)]
-- True
-- >>> unions [fromList [('a', 1)], fromList [('b', 2)]] == fromList [('a', 1), ('b', 2)]
-- True
unions :: (Ord a, Ord b) => [Relation a b] -> Relation a b
unions = F.foldl' union empty

-- | Intersection of two relations: @a@ and @b@ are related by @intersection r
-- s@ exactly when @a@ and @b@ are related by @r@ and @s@.
-- >>> fromList [('a', 1)] `intersection` fromList [('a', 1)] == fromList [('a', 1)]
-- True
-- >>> fromList [('a', 2)] `intersection` fromList [('a', 2)] == fromList [('a', 2)]
-- True
-- >>> fromList [('a', 1)] `intersection` fromList [('b', 2)] == fromList []
-- True
intersection :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
intersection r s = Relation
  { R.domain = doubleIntersect (R.domain r) (R.domain s)
  , R.range  = doubleIntersect (R.range  r) (R.range  s)
  }

ensure :: MonadPlus m => (a -> Bool) -> a -> m a
ensure p x = x <$ guard (p x)

-- This function is like M.intersectionWith S.intersection except that it
-- also removes keys that would then be associated with empty sets.
doubleIntersect :: (Ord k, Ord v) => Map k (Set v) -> Map k (Set v) -> Map k (Set v)
doubleIntersect = M.mergeWithKey
  (\_ l r -> ensure (not . S.null) (S.intersection l r))
  (const M.empty)
  (const M.empty)

-- | Insert a relation @ x @ and @ y @ in the relation @ r @
insert :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insert x y r = Relation domain' range'
  where domain' = M.insertWith S.union x (S.singleton y) (R.domain r)
        range'  = M.insertWith S.union y (S.singleton x) (R.range  r)

-- |  Delete an association in the relation.
delete :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
delete x y r = Relation
  { R.domain  = domain'
  , R.range   = range'
  }
  where domain'   = M.update (erase y) x (R.domain r)
        range'    = M.update (erase x) y (R.range  r)
        erase e s = if S.singleton e == s then Nothing else Just $ S.delete e s

-- | The Set of values associated with a value in the domain.
lookupDom :: Ord a => a -> Relation a b -> Set b
lookupDom x r = fromMaybe S.empty $ M.lookup x (R.domain r)

-- | The Set of values associated with a value in the range.
lookupRan :: Ord b => b -> Relation a b -> Set a
lookupRan y r = fromMaybe S.empty $ M.lookup y (R.range r)

-- | True if the element @ x @ exists in the domain of @ r @.
memberDom :: Ord a => a -> Relation a b -> Bool
memberDom x r = not . S.null $ lookupDom x r

-- | True if the element exists in the range.
memberRan :: Ord b => b -> Relation a b -> Bool
memberRan y r = not . S.null $ lookupRan y r

-- |
-- True if the relation @r@ is the 'empty' relation.
null :: Relation a b -> Bool
null r = M.null $ R.domain r

-- | True if the relation contains the association @x@ and @y@
member :: (Ord a, Ord b) => a -> b -> Relation a b -> Bool
member x y r = S.member y (lookupDom x r)

-- | True if the relation /does not/ contain the association @x@ and @y@
notMember :: (Ord a, Ord b) => a -> b -> Relation a b -> Bool
notMember x y r = not $ member x y r

-- | Returns the domain in the relation, as a Set, in its entirety.
dom :: Relation a b -> Set a
dom r = M.keysSet (R.domain r)

-- | Returns the range of the relation, as a Set, in its entirety.
ran :: Relation a b -> Set b
ran r = M.keysSet (R.range r)

-- | Returns the converse of the relation.
converse :: Relation a b -> Relation b a
converse r = Relation
  { R.domain = range'
  , R.range  = domain'
  }
  where range'  = R.range r
        domain' = R.domain r

-- | Restrict the domain to that of the provided set
restrictDom :: (Ord a, Ord b) => S.Set a -> Relation a b -> Relation a b
restrictDom s r = Relation
  { R.domain = M.restrictKeys (R.domain r) s
  , R.range  = M.mapMaybe (S.justUnlessEmpty . S.intersection s) (R.range r)
  }

-- | Restrict the range to that of the provided set
restrictRan :: (Ord a, Ord b) => S.Set b -> Relation a b -> Relation a b
restrictRan s r = Relation
  { R.domain  = M.mapMaybe (S.justUnlessEmpty . S.intersection s) (R.domain r)
  , R.range   = M.restrictKeys (R.range r) s
  }

-- | Restrict the domain to exclude elements of the provided set
withoutDom :: (Ord a, Ord b) => S.Set a -> Relation a b -> Relation a b
withoutDom s r = Relation
  { R.domain = M.withoutKeys (R.domain r) s
  , R.range  = M.mapMaybe (S.justUnlessEmpty . flip S.difference s) (R.range r)
  }

-- | Restrict the range to exclude elements of the provided set
withoutRan :: (Ord a, Ord b) => S.Set b -> Relation a b -> Relation a b
withoutRan s r = Relation
  { R.domain  = M.mapMaybe (S.justUnlessEmpty . flip S.difference s) (R.domain r)
  , R.range   = M.withoutKeys (R.range r) s
  }

-- | Compose two relations: right to left version.
infixr 9 <-<
(<-<) :: (Ord a, Ord b, Ord c) => Relation b c -> Relation a b -> Relation a c
a <-< b = Relation
  (compose (R.domain a) (R.domain b))
  (compose (R.range b) (R.range a))
 where
  compose a' = M.mapMaybe
    (S.justUnlessEmpty
    . fold
    . M.intersection a'
    . M.fromSet (const ())
    )

-- | Compose two relations: left to right version.
infixl 9 >->
(>->) :: (Ord a, Ord b, Ord c) => Relation a b -> Relation b c -> Relation a c
(>->) = flip (<-<)
