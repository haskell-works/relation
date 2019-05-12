module Data.Relation.Ops
  (  -- $selectops
    (|$>) -- Restrict the range according to a subset. PICA.
  , (<$|) -- Restrict the domain according to a subset. PICA.
  , (<|)  -- Domain restriction. Z.
  , (|>)  -- Range restriction. z.
  ) where

import Data.Relation.Internal (Relation)
import Data.Set               (Set)

import qualified Data.Map                   as M
import qualified Data.Relation              as R
import qualified Data.Relation.Internal     as R
import qualified Data.Relation.Internal.Set as S
import qualified Data.Set                   as S

-- $selectops
--
-- Primitive implementation for the /right selection/ and /left selection/ operators.
--
-- PICA provides both operators:
--        '|>'  and  '<|'
-- and    '|$>' and '<$|'
--
-- in this library, for working with Relations and OIS (Ordered, Inductive Sets?).
--
-- PICA exposes the operators defined here, so as not to interfere with the abstraction
-- of the Relation type and because having access to Relation hidden components is a more
-- efficient implementation of the operation of restriction.
--
-- @
--     (a <$| b) r
--
--       denotes: for every element     @b@ from the Set      @B@,
--                select an element @a@     from the Set @A@     ,
--                              if  @a@
--                   is related to      @b@
--                   in @r@
-- @
--
-- @
--     (a |$> b) r
--
--       denotes: for every element @a@      from the Set @A@    ,
--                select an element     @b@  from the Set     @B@,
--                              if  @a@
--                   is related to      @b@
--                   in @r@
-- @
--
-- With regard to domain restriction and range restriction operators
-- of the language, those are described differently and return the domain or the range.

-- |
-- @(Case b <| r a)@
(<$|) :: (Ord a, Ord b) => Set a -> Set b -> Relation a b -> Set a
(as <$| bs) r = as `S.intersection` generarAS bs
  where generarAS = S.flatten . S.map (`R.lookupRan` r)
  -- The subsets of the domain (a) associated with each @b@
  -- such that @b@ in @B@ and (b) are in the range of the relation.
  -- The expression 'S.map' returns a set of @Either (Set a)@.

-- |
-- @(Case a |> r b)@
(|$>) :: (Ord a, Ord b) => Set a -> Set b -> Relation a b -> Set b
(as |$> bs) r  = bs `S.intersection` generarBS as
  where generarBS = S.flatten . S.map (`R.lookupDom` r)

-- | Domain restriction for a relation. Modeled on z.
(<|) :: (Ord a, Ord b) => Set a -> Relation a b  -> Relation a b
s <| r = R.fromList $ concatMap (\(x, y) -> zip (repeat x) (S.toList y)) (M.toList domain')
  where domain'   = M.unions . map filtrar . S.toList $ s
        filtrar x = M.filterWithKey (\k _ -> k == x) dr
        dr        = R.domain r

-- | Range restriction for a relation. Modeled on z.
(|>) :: (Ord a, Ord b) => Relation a b -> Set b -> Relation a b
r |> t = R.fromList $ concatMap (\(x, y) -> zip (S.toList y) (repeat x)) (M.toList range')
  where range'    = M.unions . map filtrar . S.toList $ t
        filtrar x = M.filterWithKey (\k _ -> k == x) rr
        rr        = R.range r
