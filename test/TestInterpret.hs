{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestInterpret
-}

-- test/TestInterpret.hs
module TestInterpret (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import DataStored (Ast(..))
import Interpret

tests :: TestTree
tests = testGroup "Interpret tests"
  [ testGroup "basic arithmetic"
      [ testCase "(+ 1 2 3) = 6" $
          eval [] example1 @?= Right (Just (Atom 6), [])
      , testCase "(+ 5 10) = 15" $
          eval [] (List [Symbol "+", Atom 5, Atom 10]) @?= Right (Just (Atom 15), [])
      , testCase "example2 with define and -" $
          eval [] example2 @?= Right (Just (Atom 37), [("hello", Atom 42)])
      , testCase "(- 2 3) = -1" $
          eval [] (List [Symbol "-", Atom 2, Atom 3]) @?= Right (Just (Atom (-1)), [])
      , testCase "(* 2 4) = 8" $
          eval [] example3 @?= Right (Just (Atom 8), [])
      , testCase "(div 6 2) = 3" $
          eval [] example4 @?= Right (Just (Atom 3), [])
      , testCase "(mod 10 4) = 2" $
          eval [] example6 @?= Right (Just (Atom 2), [])
      ]
  , testGroup "boolean comparisons"
      [ testCase "(> 10 4)" $
          eval [] example8 @?= Right (Just (ABool True), [])
      , testCase "(> 0 10)" $
          eval [] example9 @?= Right (Just (ABool False), [])
      , testCase "(< 0 4)" $
          eval [] example10 @?= Right (Just (ABool True), [])
      , testCase "(< 10 0)" $
          eval [] example11 @?= Right (Just (ABool False), [])
      , testCase "(eq? 0 0)" $
          eval [] example12 @?= Right (Just (ABool True), [])
      , testCase "(eq? 10 0)" $
          eval [] example13 @?= Right (Just (ABool False), [])
      ]
  , testGroup "define and variables"
      [ testCase "(define x 5)" $
          eval [] (Define "x" (Atom 5)) @?= Right (Nothing, [("x", Atom 5)])
      , testCase "lookup after define" $
          let env = [("x", Atom 5)]
          in eval env (Symbol "x") @?= Right (Just (Atom 5), env)
      , testCase "exampleDefineTest" $
          eval [] exampleDefineTest @?= Right (Just (Atom 42), [("test", Atom 42)])
      ]
  , testGroup "if expressions"
      [ testCase "(if #t 1 2)" $
          eval [] (If (ABool True) (Atom 1) (Atom 2)) @?= Right (Just (Atom 1), [])
      , testCase "(if #f 1 2)" $
          eval [] (If (ABool False) (Atom 1) (Atom 2)) @?= Right (Just (Atom 2), [])
      , testCase "exampleTest" $
          eval [] exampleTest @?= Right (Just (Atom 1), [])
      ]
  , testGroup "atoms and bools"
      [ testCase "Atom 42" $
          eval [] (Atom 42) @?= Right (Just (Atom 42), [])
      , testCase "ABool True" $
          eval [] (ABool True) @?= Right (Just (ABool True), [])
      ]
  , testGroup "lambda expressions"
      [ testCase "lambda definition" $
          let lam = Lambda ["x"] (Atom 1)
          in eval [] lam @?= Right (Just lam, [])
      , testCase "lambda application (example14)" $
          eval [] example14 @?= Right (Just (Atom 5), [])
      , testCase "exampleDefine" $
          let expected = Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"])
          in eval [] exampleDefine @?= Right (Nothing, [("caca", expected)])
      , testCase "exampleMultiDefine" $
          let helloLam = Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"])
              worldLam = Lambda ["x","y"] (List [Symbol "*", Symbol "x", Symbol "y"])
              env = [("world", worldLam), ("hello", helloLam)]
          in eval [] exampleMultiDefine @?= Right (Nothing, env)
      ]
  , testGroup "call expressions"
      [ testCase "builtin operators" $
          eval [] (Call (Symbol "+") [Atom 1, Atom 2]) @?= Right (Just (Atom 3), [])
      , testCase "user-defined function" $
          let env = [("add", Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"]))]
          in eval env (Call (Symbol "add") [Atom 2, Atom 3]) @?= Right (Just (Atom 5), env)
      , testCase "exampleDefineCall" $
          let helloLam = Lambda ["x"] (Call (Symbol "+") [Symbol "x", Atom 1])
          in eval [] exampleDefineCall @?= Right (Just (Atom 3), [("hello", helloLam)])
      ]
  , testGroup "complex lambda examples"
      [ testCase "exampleLambdaWithCall" $
          let lam = exampleLambdaWithCall
          in eval [] lam @?= Right (Just lam, [])
      , testCase "apply exampleLambdaWithCall" $
          let app = List [exampleLambdaWithCall, Atom 3, Atom 4]
          in eval [] app @?= Right (Just (Atom 12), [])
      , testCase "exampleLambdaWithIf" $
          let lam = exampleLambdaWithIf
          in eval [] lam @?= Right (Just lam, [])
      , testCase "apply exampleLambdaWithIf positive" $
          let app = List [exampleLambdaWithIf, Atom 5]
          in eval [] app @?= Right (Just (Atom 5), [])
      , testCase "apply exampleLambdaWithIf negative" $
          let app = List [exampleLambdaWithIf, Atom (-3)]
          in eval [] app @?= Right (Just (Atom 0), [])
      ]
  ]

example1, example2, example3, example4, example5, example6, example7 :: Ast
example8, example9, example10, example11, example12, example13, example14 :: Ast
exampleDefine, exampleMultiDefine, exampleCall, exampleDefineCall :: Ast
exampleTest, exampleDefineTest, exampleLambdaWithCall, exampleLambdaWithIf, exampleNestedLambda :: Ast

example1 = List [Symbol "+", Atom 1, Atom 2, Atom 3]
example2 = List [Define "hello" (Atom 42), Symbol "-", Symbol "hello", Atom 2, Atom 3]
example3 = List [Symbol "*", Atom 2, Atom 4]
example4 = List [Symbol "div", Atom 6, Atom 2]
example5 = List [Symbol "div", Atom 6, Atom 0]
example6 = List [Symbol "mod", Atom 10, Atom 4]
example7 = List [Symbol "mod", Atom 10, Atom 0]
example8 = List [Symbol ">", Atom 10, Atom 4]
example9 = List [Symbol ">", Atom 0, Atom 10]
example10 = List [Symbol "<", Atom 0, Atom 4]
example11 = List [Symbol "<", Atom 10, Atom 0]
example12 = List [Symbol "eq?", Atom 0, Atom 0]
example13 = List [Symbol "eq?", Atom 10, Atom 0]
example14 = List [Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"]), Atom 2, Atom 3]

exampleDefine = Define "caca" (Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"]))
exampleMultiDefine = List [Define "hello" (Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"])),
                           Define "world" (Lambda ["x","y"] (List [Symbol "*", Symbol "x", Symbol "y"]))]
exampleCall = Call (Symbol "hello") [Atom 2, Atom 3]
exampleDefineCall = List [Define "hello" (Lambda ["x"] (Call (Symbol "+") [Symbol "x", Atom 1])),
                          Call (Symbol "hello") [Atom 2]]
exampleTest = If (List [Symbol ">", Atom 10, Atom 4]) (Atom 1) (Atom 2)
exampleDefineTest = List [Define "test" (Atom 42), Symbol "test"]

exampleLambdaWithCall = Lambda ["x","y"] (Call (Symbol "*") [Symbol "x", Symbol "y"])
exampleLambdaWithIf = Lambda ["x"] (If (List [Symbol ">", Symbol "x", Atom 0]) (Symbol "x") (Atom 0))
exampleNestedLambda = Lambda ["f","x"] (Call (Symbol "f") [Symbol "x"])
