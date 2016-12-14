module Main where

import Control.Monad (forM_)
import Prelude hiding (pi)

import Language
import Parser

τFlip :: Type ()
τFlip = pi [ ("A", set), ("B", set), ("C", set) ] $
     (var "A" --> var "B" --> var "C") -->
     (var "B" --> var "A" --> var "C")

tFlip :: Term ()
tFlip = lam ["A", "B", "C", "f", "b", "a"] $
        var "f" $$ var "a" $$ var "b"

τIll :: Term ()
τIll = pi [("A", set), ("B", set), ("C", set)] $
     (var "A" --> var "B" --> var "C") -->
     (var "B" --> var "A" --> var "C")

tIll :: Term ()
tIll = lam ["A", "B", "C", "f", "b", "a"] $
       var "f" $$ var "a" $$ var "a"

tIll2 :: Term ()
tIll2 = lam ["A", "B", "C", "f", "b", "a"] $
       var "a" $$ var "b"

τHole :: Type ()
τHole = pi [ ("A", set), ("B", set), ("C", set) ] $
     (var "A" --> var "B" --> var "C") -->
     (var "B" --> var "A" --> var "C")

tHole :: Term ()
tHole = lam ["A", "B", "C", "f", "b", "a"] $ hole

natrecT :: Type ()
natrecT =
  pi [("T", var "nat" --> set)] $
  (var "T" $$ var "O") -->
  (pi [("n", var "nat")] $
   (var "T" $$ var "n") --> var "T" $$ (var "S" $$ var "n")) -->
  (pi [("n", var "nat")] $ var "T" $$ var "n")

γ1 :: TypCtxt ()
γ1 = reverse
     [ ("nat", set)
     , ("O", var "nat")
     , ("S", var "nat" --> var "nat")
     , ("natrec", natrecT)
     ]

τPlus :: Type ()
τPlus = var "nat" --> var "nat" --> var "nat"

tPlusBad :: Term ()
tPlusBad =
  var "natrec" $$
  (Lam defaultValue Nothing (var "nat")) $$
  var "O" $$
  (var "S" $$ var "O") -- wrong, should be lambda

tPlus :: Term ()
tPlus =
  var "natrec" $$
  (Lam defaultValue Nothing (var "nat")) $$
  var "O" $$
  (lam ["n", "Tn"] $ var "S" $$ var "Tn")

mainTraceStep :: IO ()
mainTraceStep = do
  let trace = tcTrace . mkWork $ annot tFlip τFlip
  forM_ trace $ \ t -> do
    putStrLn $ show t

main :: IO ()
main = do
  mainTraceStep
  --putStrLn . pprint . tcTraceResult . mkWork $ annot tFlip τFlip
