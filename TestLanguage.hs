{-# LANGUAGE DeriveDataTypeable #-}

module TestLanguage where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Typeable
import Test.QuickCheck hiding (Failure, Success)

import Parser
import Language

arbVar :: Gen String
arbVar = elements $ ["a", "b", "c"]

arbMaybeVar :: Gen (Maybe String)
arbMaybeVar = elements $ Nothing : map Just ["a", "b", "c"]

instance (Arbitrary a, Eq a, Typeable a) => Arbitrary (Term a) where
  arbitrary =
    frequency
    [ (10, liftM Type arbitrary)
    , (branchF, liftM4 Pi arbitrary arbMaybeVar arbitrary arbitrary)
    , (leafF, liftM2 Var arbitrary arbVar)
    , (branchF, liftM3 Lam arbitrary arbMaybeVar arbitrary)
    , (branchF, liftM3 App arbitrary arbitrary arbitrary)
    , (0, liftM4 Let arbitrary arbMaybeVar arbitrary arbitrary)
    , (1, liftM3 Annot arbitrary arbitrary arbitrary)
    , (leafF `div` 2, liftM Hole arbitrary)
    ]
    where
      branchF = 66
      leafF   = 100
  shrink t = delete t (shrink' t)
    where
      shrink' :: Term a -> [Term a]
      shrink' (Pi a n τ t) = τ : t : (Pi a n <$> shrink' τ <*> shrink' t)
      shrink' (Lam a n t) = t : (Lam a n <$> shrink' t)
      shrink' (App a t1 t2) = t1 : t2 : (App a <$> shrink' t1 <*> shrink' t2)
      shrink' (Let a n t1 t2) = t1 : t2 : (Let a n <$> shrink' t1 <*> shrink' t2)
      shrink' (Annot a t τ) = t : (Annot a <$> shrink' t <*> shrink' τ)
      shrink' t = [t]

terms :: Gen (Term ())
terms = arbitrary

normalForms :: Gen (Term ())
normalForms = fmap redβ terms

prop_red_idempotent :: Eq a => Term a -> Bool
prop_red_idempotent t = redβ (redβ t) == redβ t

prop_red_noredex :: Term () -> Bool
prop_red_noredex t = case redβ t of
  App _ (Lam _ _ _) _ -> False
  Pi _ _ τ1 τ2 -> prop_red_noredex τ1 && prop_red_noredex τ2
  Lam _ _ t -> prop_red_noredex t
  App _ t1 t2 -> prop_red_noredex t1 && prop_red_noredex t2
  Let _ _ t1 t2 -> prop_red_noredex t1 && prop_red_noredex t2
  Annot _ t _ -> prop_red_noredex t
  _ -> True

prop_parser1 :: Term () -> Bool
prop_parser1 t = pprint t == pprint (parseTerm (pprint t))

prop_parser2 :: Term () -> Bool
prop_parser2 t = t == parseTerm (pprint t)
