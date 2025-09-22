{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestInterpret
-}

module TestInterpret (spec) where

import Test.Hspec
import Interpret
import DataStored (Ast(..))

spec :: Spec
spec = do
  describe "basic arithmetic" $ do
    it "evaluates (+ 1 2 3) = 6" $ do
      eval [] example1 `shouldBe` Just (Atom 6, [])

    it "evaluates (* 2 4) = 8" $ do
      eval [] example3 `shouldBe` Just (Atom 8, [])

    it "evaluates (div 6 2) = 3" $ do
      eval [] example4 `shouldBe` Just (Atom 3, [])

    it "evaluates (mod 10 4) = 2" $ do
      eval [] example6 `shouldBe` Just (Atom 2, [])

  describe "boolean comparisons" $ do
    it "evaluates (> 10 4) = True" $ do
      eval [] example8 `shouldBe` Just (ABool True, [])

    it "evaluates (< 0 4) = True" $ do
      eval [] example10 `shouldBe` Just (ABool True, [])
