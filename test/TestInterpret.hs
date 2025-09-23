{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestInterpret
-}

module TestInterpret (spec) where

import Control.Exception (evaluate)
import DataStored (Ast(..))
import Test.Hspec
import Interpret

spec :: Spec
spec = do
  describe "basic arithmetic" $ do
    it "evaluates (+ 1 2 3) = 6" $ do
      eval [] example1 `shouldBe` Just (Atom 6, [])

    it "evaluates (+ 5 10) = 15" $ do
      eval [] (List [Symbol "+", Atom 5, Atom 10]) `shouldBe` Just (Atom 15, [])

    it "evaluates (- hello 2 3) = Nothing" $ do
      eval [] example2 `shouldBe` Nothing

    it "evaluates (- 2 3) = -1" $ do
      eval [] (List [Symbol "-", Atom 2, Atom 3]) `shouldBe` Just (Atom (-1), [])

    it "evaluates (* 2 4) = 8" $ do
      eval [] example3 `shouldBe` Just (Atom 8, [])

    it "evaluates (div 6 2) = 3" $ do
      eval [] example4 `shouldBe` Just (Atom 3, [])

    it "evaluates (div 6 0) = error call" $ do
      evaluate (eval [] example5) `shouldThrow` anyErrorCall

    it "evaluates (mod 10 4) = 2" $ do
      eval [] example6 `shouldBe` Just (Atom 2, [])

    it "evaluates (mod 10 0) = error call" $ do
      evaluate (eval [] example7) `shouldThrow` anyErrorCall

  describe "boolean comparisons" $ do
    it "evaluates (> 10 4) = True" $ do
      eval [] example8 `shouldBe` Just (ABool True, [])

    it "evaluates (> 0 10) = False" $ do
      eval [] example9 `shouldBe` Just (ABool False, [])

    it "evaluates (< 0 4) = True" $ do
      eval [] example10 `shouldBe` Just (ABool True, [])

    it "evaluates (< 10 0) = False" $ do
      eval [] example11 `shouldBe` Just (ABool False, [])

    it "evaluates (eq? 0 0) = True" $ do
      eval [] example12 `shouldBe` Just (ABool True, [])

    it "evaluates (eq? 10 0) = False" $ do
      eval [] example13 `shouldBe` Just (ABool False, [])

  describe "define and variables" $ do
    it "evaluates (define x 5)" $ do
      eval [] (Define "x" (Atom 5)) `shouldBe` Just (Symbol "x", [("x", Atom 5)])

    it "evaluates symbol lookup after define" $ do
      let env = [("x", Atom 5)]
      eval env (Symbol "x") `shouldBe` Just (Atom 5, env)

  describe "if expressions" $ do
    it "evaluates (if #t 1 2) = 1" $ do
      eval [] (If (ABool True) (Atom 1) (Atom 2)) `shouldBe` Just (Atom 1, [])

    it "evaluates (if #f 1 2) = 2" $ do
      eval [] (If (ABool False) (Atom 1) (Atom 2)) `shouldBe` Just (Atom 2, [])

  describe "atoms and bools" $ do
    it "evaluates raw Atom" $ do
      eval [] (Atom 42) `shouldBe` Just (Atom 42, [])

    it "evaluates raw ABool" $ do
      eval [] (ABool True) `shouldBe` Just (ABool True, [])

  describe "lambda and unsupported" $ do
    it "evaluates Lambda" $ do
      let lam = Lambda ["x"] (Atom 1)
      eval [] lam `shouldBe` Just (lam, [])

    it "rejects Call expressions" $ do
      eval [] (Call (Symbol "f") [Atom 1]) `shouldBe` Nothing

    it "rejects malformed List" $ do
      eval [] (List [Symbol "define", Atom 1]) `shouldBe` Nothing
