module Data.RelationSpec
  ( spec
  ) where

import Data.Relation.Ops
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.List              as L
import qualified Data.Map               as M
import qualified Data.Relation          as DR
import qualified Data.Relation.Internal as DR
import qualified Data.Set               as S
import qualified Hedgehog.Gen           as G
import qualified Hedgehog.Range         as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

e :: DR.Relation String String
e = DR.fromList
  [ ("Rebeca" , "History"        )
  , ("Rebeca" , "Mathematics"    )
  , ("Rolando", "Religion"       )
  , ("Rolando", "Comunication"   )
  , ("Teresa" , "Religion"       )
  , ("Teresa" , "Architecture"   )
  , ("Antonio", "History"        )
  ]

rebecaE :: S.Set String
rebecaE = (S.singleton "Rebeca" |$> DR.ran e) e

takingreligion :: S.Set String
takingreligion = (DR.dom e <$| S.singleton "Religion") e

others :: S.Set String
others = (takingreligion |$> DR.ran e) e

takingreligion2 :: DR.Relation String String
takingreligion2 = e |> S.singleton "Religion"

twoStudents :: DR.Relation String String
twoStudents = (<|) (S.union (S.singleton "Rolando") (S.singleton "Teresa")) e

id1 :: S.Set String -> (Bool, S.Set String)
id1 s = (v1 == v2, v1)
  where v1 = (DR.dom  e |$> s) e
        v2 =  DR.ran (e |>  s)

id2 :: S.Set String -> (Bool, S.Set String)
id2 s = (v1 == v2, v1)
  where v1 = (DR.dom  e <$| s) e
        v2 =  DR.dom (e |>  s)

id3 :: S.Set String -> (Bool, S.Set String)
id3 s = (v1 == v2, v1)
  where v1 = (s       <$| DR.ran e) e
        v2 = DR.dom (s <|  e)

id4 :: S.Set String -> (Bool, S.Set String)
id4 s = (v1 == v2, v2)
  where v1 = (s       |$> DR.ran e) e
        v2 = DR.ran (s <|  e)

religion :: S.Set String
religion = S.singleton "Religion"  -- has students

teresa :: S.Set String
teresa = S.singleton "Teresa" -- enrolled

spec :: Spec
spec = describe "Data.RelationSpec" $ do
  describe "Unit tests" $ do
    it "fromList" $ requireTest $ do
      e ===  DR.Relation
        { DR.domain = M.fromList
          [ ("Antonio"      , S.fromList ["History"                 ])
          , ("Rebeca"       , S.fromList ["History", "Mathematics"  ])
          , ("Rolando"      , S.fromList ["Comunication", "Religion"])
          , ("Teresa"       , S.fromList ["Architecture", "Religion"])
          ]
        , DR.range = M.fromList
          [ ("Architecture" , S.fromList ["Teresa"                  ])
          , ("Comunication" , S.fromList ["Rolando"                 ])
          , ("History"      , S.fromList ["Antonio", "Rebeca"       ])
          , ("Mathematics"  , S.fromList ["Rebeca"                  ])
          , ("Religion"     , S.fromList ["Rolando", "Teresa"       ])
          ]
        }
    it "singleton & range" $ requireTest $ do
      rebecaE === S.fromList ["History", "Mathematics"]
    it "singleton & domain" $ requireTest $ do
      takingreligion === S.fromList ["Rolando", "Teresa"]
    it "(|$>)" $ requireTest $ do
      others === S.fromList ["Architecture", "Comunication", "Religion"]
    it "test1" $ requireTest $ do
      (takingreligion <$| DR.ran e) e === takingreligion
    it "Exploring |>" $ requireTest $ do
      takingreligion2 === DR.Relation
        { DR.domain = M.fromList
          [ ("Rolando"  , S.fromList ["Religion"          ])
          , ("Teresa"   , S.fromList ["Religion"          ])
          ]
        , DR.range = M.fromList
          [ ("Religion" , S.fromList ["Rolando", "Teresa" ])
          ]
        }
    it "twoStudents" $ requireTest $ do
      twoStudents === DR.Relation
        { DR.domain = M.fromList
          [ ("Rolando"      , S.fromList ["Comunication", "Religion"])
          , ("Teresa"       , S.fromList ["Architecture", "Religion"])
          ]
        , DR.range = M.fromList
          [ ("Architecture" , S.fromList ["Teresa"                  ])
          , ("Comunication" , S.fromList ["Rolando"                 ])
          , ("Religion"     , S.fromList ["Rolando", "Teresa"       ])
          ]
        }
    it "test 2" $ requireTest $ do
      (|$>) (S.union (S.singleton "Rolando") (S.singleton "Teresa")) (DR.ran e) e === S.fromList ["Architecture", "Comunication", "Religion"]
    it "test 3" $ requireTest $ do
      id1 religion === (True, S.fromList ["Religion"])
    it "test 4" $ requireTest $ do
      id2 religion === (True, S.fromList ["Rolando", "Teresa"])
    it "test 5" $ requireTest $ do
      id3 teresa === (True, S.fromList ["Teresa"])
    it "test 6" $ requireTest $ do
      id4 teresa === (True, S.fromList ["Architecture", "Religion"])
    it "test 7"  $ requireTest $ do
      (DR.dom e |$> religion) e === DR.ran (e |> religion)
    it "test 8"  $ requireTest $ do
      (DR.dom e <$| religion) e === DR.dom (e |> religion)
    it "test 9"  $ requireTest $ do
      (teresa  <$| DR.ran e) e === DR.dom (teresa <| e)
    it "test 10"  $ requireTest $ do
      (teresa |$> DR.ran e) e === DR.ran (teresa <| e)

  describe "property tests" $ do
    it "List roundtrip" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha
      L.sort (DR.toList (DR.fromList as)) === L.sort as
    it "Full domain restriction" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha

      DR.restrictDom S.empty (DR.fromList as) === DR.empty
    it "Full range restriction" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha

      DR.restrictRan S.empty (DR.fromList as) === DR.empty
    it "No domain restriction" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha
      let r = DR.fromList as

      DR.restrictDom (DR.dom r) r === r
    it "No range restriction" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha
      let r = DR.fromList as
      DR.restrictRan (DR.ran r) r === r
    it "Full domain without" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha
      let r = DR.fromList as
      DR.withoutDom S.empty r === r
    it "Full range without" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha
      let r = DR.fromList as
      DR.withoutRan S.empty r === r
    it "No domain without" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha
      let r = DR.fromList as

      DR.withoutDom (DR.dom r) r === DR.empty
    it "No range without" $ require $ property $ do
      as <- forAll $ G.list (R.linear 0 10) $ (,)
        <$> G.int R.constantBounded
        <*> G.alpha
      let r = DR.fromList as
      DR.withoutRan (DR.ran r) r === DR.empty
