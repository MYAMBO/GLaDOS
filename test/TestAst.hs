{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestAst
-}

module TestAst (spec) where

import Test.Hspec
import Ast.Ast
import DataStored (Ast(..))

spec :: Spec
spec = do
  describe "sexprToAST" $ do

    describe "Atom conversion" $ do
      it "converts a positive SAtom to an Atom" $ do
        sexprToAST (SAtom 42) `shouldBe` Just (Atom 42)
      it "converts a negative SAtom to an Atom" $ do
        sexprToAST (SAtom (-10)) `shouldBe` Just (Atom (-10))

    describe "Boolean conversion" $ do
      it "converts SSymbol \"#t\" to ABool True" $ do
        sexprToAST (SSymbol "#t") `shouldBe` Just (ABool True)
      it "converts SSymbol \"#f\" to ABool False" $ do
        sexprToAST (SSymbol "#f") `shouldBe` Just (ABool False)

    describe "Symbol conversion" $ do
      it "converts an SSymbol to a Symbol" $ do
        sexprToAST (SSymbol "my-var") `shouldBe` Just (Symbol "my-var")

    describe "Define conversion" $ do
      it "converts a simple variable definition" $ do
        let input = SList [SSymbol "define", SSymbol "x", SAtom 5]
        let expected = Just (Define "x" (Atom 5))
        sexprToAST input `shouldBe` expected
      it "converts a function definition (lambda shorthand)" $ do
        let input = SList [SSymbol "define", SList [SSymbol "add", SSymbol "x", SSymbol "y"], SAtom 1]
        let expected = Just (Define "add" (Lambda ["x", "y"] (Atom 1)))
        sexprToAST input `shouldBe` expected

    describe "If conversion" $ do
      it "converts a valid if expression" $ do
        let input = SList [SSymbol "if", SSymbol "#t", SAtom 1, SAtom 2]
        let expected = Just (If (ABool True) (Atom 1) (Atom 2))
        sexprToAST input `shouldBe` expected
      it "converts a nested if expression" $ do
        let innerIf = SList [SSymbol "if", SSymbol "cond", SAtom 1, SAtom 2]
        let input = SList [SSymbol "if", SSymbol "#f", innerIf, SAtom 3]
        let expected = Just (If (ABool False) (If (Symbol "cond") (Atom 1) (Atom 2)) (Atom 3))
        sexprToAST input `shouldBe` expected

    describe "Lambda conversion" $ do
      it "converts a valid lambda expression" $ do
        let input = SList [SSymbol "lambda", SList [SSymbol "x", SSymbol "y"], SAtom 1]
        let expected = Just (Lambda ["x", "y"] (Atom 1))
        sexprToAST input `shouldBe` expected
      it "converts a lambda with an empty argument list" $ do
        let input = SList [SSymbol "lambda", SList [], SAtom 42]
        let expected = Just (Lambda [] (Atom 42))
        sexprToAST input `shouldBe` expected

    describe "Call conversion" $ do
      it "converts a function call with arguments" $ do
        let input = SList [SSymbol "+", SAtom 1, SAtom 2]
        let expected = Just (Call (Symbol "+") [Atom 1, Atom 2])
        sexprToAST input `shouldBe` expected
      it "converts a function call with no arguments" $ do
        let input = SList [SSymbol "get-time"]
        let expected = Just (Call (Symbol "get-time") [])
        sexprToAST input `shouldBe` expected
      it "converts a nested function call" $ do
        let innerCall = SList [SSymbol "*", SAtom 2, SAtom 3]
        let input = SList [SSymbol "+", SAtom 1, innerCall]
        let expected = Just (Call (Symbol "+") [Atom 1, Call (Symbol "*") [Atom 2, Atom 3]])
        sexprToAST input `shouldBe` expected
      it "converts a call where the callee is a lambda expression" $ do
        let lambda = SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "x"]
        let input = SList [lambda, SAtom 5]
        let expected = Just (Call (Lambda ["x"] (Symbol "x")) [Atom 5])
        sexprToAST input `shouldBe` expected

    describe "Empty List conversion" $ do
      it "fails to convert an empty list" $ do
        sexprToAST (SList []) `shouldBe` Nothing
