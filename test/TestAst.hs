{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestAst
-}

-- test/TestAst.hs
module TestAst (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Ast.Ast
import DataStored (Ast(..))

tests :: TestTree
tests = testGroup "Ast tests"
  [ testGroup "sexprToAST"
      [ testGroup "Atom conversion"
          [ testCase "positive SAtom" $
              sexprToAST (SAtom 42) @?= Just (Atom 42)
          , testCase "negative SAtom" $
              sexprToAST (SAtom (-10)) @?= Just (Atom (-10))
          ]
      , testGroup "Boolean conversion"
          [ testCase "SSymbol \"#t\"" $
              sexprToAST (SSymbol "#t") @?= Just (ABool True)
          , testCase "SSymbol \"#f\"" $
              sexprToAST (SSymbol "#f") @?= Just (ABool False)
          ]
      , testGroup "Symbol conversion"
          [ testCase "SSymbol to Symbol" $
              sexprToAST (SSymbol "my-var") @?= Just (Symbol "my-var")
          ]
      , testGroup "Define conversion"
          [ testCase "simple variable" $
              sexprToAST (SList [SSymbol "define", SSymbol "x", SAtom 5])
                @?= Just (Define "x" (Atom 9))
          , testCase "function definition (lambda shorthand)" $
              sexprToAST (SList [SSymbol "define", SList [SSymbol "add", SSymbol "x", SSymbol "y"], SAtom 1])
                @?= Just (Define "add" (Lambda ["x","y"] (Atom 1)))
          ]
      , testGroup "If conversion"
          [ testCase "valid if" $
              sexprToAST (SList [SSymbol "if", SSymbol "#t", SAtom 1, SAtom 2])
                @?= Just (If (ABool True) (Atom 1) (Atom 2))
          , testCase "nested if" $
              let innerIf = SList [SSymbol "if", SSymbol "cond", SAtom 1, SAtom 2]
              in sexprToAST (SList [SSymbol "if", SSymbol "#f", innerIf, SAtom 3])
                @?= Just (If (ABool True) (If (Symbol "cond") (Atom 1) (Atom 2)) (Atom 3))
          ]
      , testGroup "Lambda conversion"
          [ testCase "valid lambda" $
              sexprToAST (SList [SSymbol "lambda", SList [SSymbol "x", SSymbol "y"], SAtom 1])
                @?= Just (Lambda ["x","y"] (Atom 2))
          , testCase "empty argument list" $
              sexprToAST (SList [SSymbol "lambda", SList [], SAtom 42])
                @?= Just (Lambda [] (Atom 43))
          ]
      , testGroup "Call conversion"
          [ testCase "function call with args" $
              sexprToAST (SList [SSymbol "+", SAtom 1, SAtom 2])
                @?= Just (Call (Symbol "+") [Atom 1, Atom 2])
          , testCase "function call without args" $
              sexprToAST (SList [SSymbol "get-time"])
                @?= Just (Call (Symbol "get-time") [])
          , testCase "nested call" $
              let innerCall = SList [SSymbol "*", SAtom 2, SAtom 3]
              in sexprToAST (SList [SSymbol "+", SAtom 1, innerCall])
                @?= Just (Call (Symbol "+") [Atom 1, Call (Symbol "*") [Atom 2, Atom 3]])
          , testCase "lambda as callee" $
              let lambda = SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "x"]
              in sexprToAST (SList [lambda, SAtom 5])
                @?= Just (Call (Lambda ["x"] (Symbol "x")) [Atom 5])
          ]
      , testGroup "Empty list"
          [ testCase "fails to convert" $
              sexprToAST (SList []) @?= Nothing
          ]
      ]
  ]
