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
      eval [] example1 `shouldBe` Just (Just (Atom 6), [])

    it "evaluates (+ 5 10) = 15" $ do
      eval [] (List [Symbol "+", Atom 5, Atom 10]) `shouldBe` Just (Just (Atom 15), [])

    it "evaluates example2 with define and subtraction" $ do
      let expected = Just (Just (Atom 37), [("hello", Atom 42)])
      eval [] example2 `shouldBe` expected

    it "evaluates (- 2 3) = -1" $ do
      eval [] (List [Symbol "-", Atom 2, Atom 3]) `shouldBe` Just (Just (Atom (-1)), [])

    it "evaluates (* 2 4) = 8" $ do
      eval [] example3 `shouldBe` Just (Just (Atom 8), [])

    it "evaluates (div 6 2) = 3" $ do
      eval [] example4 `shouldBe` Just (Just (Atom 3), [])

--    it "evaluates (div 6 0) = error call" $ do
--      evaluate (eval [] example5) `shouldThrow` anyErrorCall

    it "evaluates (mod 10 4) = 2" $ do
      eval [] example6 `shouldBe` Just (Just (Atom 2), [])

--    it "evaluates (mod 10 0) = error call" $ do
--      evaluate (eval [] example7) `shouldThrow` anyErrorCall

  describe "boolean comparisons" $ do
    it "evaluates (> 10 4) = True" $ do
      eval [] example8 `shouldBe` Just (Just (ABool True), [])

    it "evaluates (> 0 10) = False" $ do
      eval [] example9 `shouldBe` Just (Just (ABool False), [])

    it "evaluates (< 0 4) = True" $ do
      eval [] example10 `shouldBe` Just (Just (ABool True), [])

    it "evaluates (< 10 0) = False" $ do
      eval [] example11 `shouldBe` Just (Just (ABool False), [])

    it "evaluates (eq? 0 0) = True" $ do
      eval [] example12 `shouldBe` Just (Just (ABool True), [])

    it "evaluates (eq? 10 0) = False" $ do
      eval [] example13 `shouldBe` Just (Just (ABool False), [])

  describe "define and variables" $ do
    it "evaluates (define x 5)" $ do
      eval [] (Define "x" (Atom 5)) `shouldBe` Just (Nothing, [("x", Atom 5)])

    it "evaluates symbol lookup after define" $ do
      let env = [("x", Atom 5)]
      eval env (Symbol "x") `shouldBe` Just (Just (Atom 5), env)

    it "evaluates exampleDefineTest" $ do
      eval [] exampleDefineTest `shouldBe` Just (Just (Atom 42), [("test", Atom 42)])

  describe "if expressions" $ do
    it "evaluates (if #t 1 2) = 1" $ do
      eval [] (If (ABool True) (Atom 1) (Atom 2)) `shouldBe` Just (Just (Atom 1), [])

    it "evaluates (if #f 1 2) = 2" $ do
      eval [] (If (ABool False) (Atom 1) (Atom 2)) `shouldBe` Just (Just (Atom 2), [])

    it "evaluates exampleTest with condition evaluation" $ do
      eval [] exampleTest `shouldBe` Just (Just (Atom 1), [])

  describe "atoms and bools" $ do
    it "evaluates raw Atom" $ do
      eval [] (Atom 42) `shouldBe` Just (Just (Atom 42), [])

    it "evaluates raw ABool" $ do
      eval [] (ABool True) `shouldBe` Just (Just (ABool True), [])

  describe "lambda expressions" $ do
    it "evaluates Lambda definition" $ do
      let lam = Lambda ["x"] (Atom 1)
      eval [] lam `shouldBe` Just (Just lam, [])

    it "evaluates lambda application (example14)" $ do
      eval [] example14 `shouldBe` Just (Just (Atom 5), [])

    it "evaluates exampleDefine" $ do
      let expected_lambda = Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"])
      eval [] exampleDefine `shouldBe` Just (Nothing, [("caca", expected_lambda)])

    it "evaluates exampleMultiDefine" $ do
      let hello_lambda = Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"])
      let world_lambda = Lambda ["x","y"] (List [Symbol "*", Symbol "x", Symbol "y"])
      let expected_env = [("world", world_lambda), ("hello", hello_lambda)]
      eval [] exampleMultiDefine `shouldBe` Just (Nothing, expected_env)

  describe "call expressions" $ do
    it "evaluates Call with built-in operators" $ do
      eval [] (Call (Symbol "+") [Atom 1, Atom 2]) `shouldBe` Just (Just (Atom 3), [])

    it "evaluates Call with user-defined function" $ do
      let env = [("add", Lambda ["x", "y"] (List [Symbol "+", Symbol "x", Symbol "y"]))]
      eval env (Call (Symbol "add") [Atom 2, Atom 3]) `shouldBe` Just (Just (Atom 5), env)

    it "evaluates exampleDefineCall" $ do
      let hello_lambda = Lambda ["x"] (Call (Symbol "+") [Symbol "x", Atom 1])
      eval [] exampleDefineCall `shouldBe` Just (Just (Atom 3), [("hello", hello_lambda)])

  describe "complex lambda examples" $ do
    it "evaluates exampleLambdaWithCall" $ do
      let lam = exampleLambdaWithCall
      eval [] lam `shouldBe` Just (Just lam, [])

    it "applies exampleLambdaWithCall" $ do
      let application = List [exampleLambdaWithCall, Atom 3, Atom 4]
      eval [] application `shouldBe` Just (Just (Atom 12), [])

    it "evaluates exampleLambdaWithIf" $ do
      let lam = exampleLambdaWithIf
      eval [] lam `shouldBe` Just (Just lam, [])

    it "applies exampleLambdaWithIf with positive number" $ do
      let application = List [exampleLambdaWithIf, Atom 5]
      eval [] application `shouldBe` Just (Just (Atom 5), [])

    it "applies exampleLambdaWithIf with negative number" $ do
      let application = List [exampleLambdaWithIf, Atom (-3)]
      eval [] application `shouldBe` Just (Just (Atom 0), [])


example1 :: Ast
example1 = List [Symbol "+", Atom 1, Atom 2, Atom 3]

example2 :: Ast
example2 = List 
    [
        Define "hello" (Atom 42)
        , Symbol "-", Symbol "hello", Atom 2, Atom 3
    ]

example3 :: Ast
example3 = List [Symbol "*", Atom 2, Atom 4]

example4 :: Ast
example4 = List [Symbol "div", Atom 6, Atom 2]

example5 :: Ast
example5 = List [Symbol "div", Atom 6, Atom 0]

example6 :: Ast
example6 = List [Symbol "mod", Atom 10, Atom 4]

example7 :: Ast
example7 = List [Symbol "mod", Atom 10, Atom 0]

example8 :: Ast
example8 = List [Symbol ">", Atom 10, Atom 4]

example9 :: Ast
example9 = List [Symbol ">", Atom 0, Atom 10]

example10 :: Ast
example10 = List [Symbol "<", Atom 0, Atom 4]

example11 :: Ast
example11 = List [Symbol "<", Atom 10, Atom 0]

example12 :: Ast
example12 = List [Symbol "eq?", Atom 0, Atom 0]

example13 :: Ast
example13 = List [Symbol "eq?", Atom 10, Atom 0]

example14 :: Ast
example14 = List [Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"]), Atom 2, Atom 3]

exampleDefine :: Ast
exampleDefine = Define "caca" (Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"]))

exampleMultiDefine :: Ast
exampleMultiDefine = List
    [ Define "hello" (Lambda ["x","y"] (List [Symbol "+", Symbol "x", Symbol "y"]))
    , Define "world" (Lambda ["x","y"] (List [Symbol "*", Symbol "x", Symbol "y"]))
    ]

exampleCall :: Ast
exampleCall = Call (Symbol "hello") [Atom 2, Atom 3]

-- Fixed example with proper Call syntax
exampleDefineCall :: Ast
exampleDefineCall =
    List
      [ Define "hello" (Lambda ["x"] (Call (Symbol "+") [Symbol "x", Atom 1]))
      , Call (Symbol "hello") [Atom 2]
      ]

exampleTest :: Ast
exampleTest =
    If (List [Symbol ">", Atom 10, Atom 4])
       (Atom 1)
       (Atom 2)

exampleDefineTest :: Ast
exampleDefineTest =
    List
      [ Define "test" (Atom 42)
      , Symbol "test"
      ]

-- Additional examples to test lambda flexibility
exampleLambdaWithCall :: Ast
exampleLambdaWithCall = Lambda ["x", "y"] (Call (Symbol "*") [Symbol "x", Symbol "y"])

exampleLambdaWithIf :: Ast
exampleLambdaWithIf = Lambda ["x"] (If (List [Symbol ">", Symbol "x", Atom 0])
                                      (Symbol "x")
                                      (Atom 0))

exampleNestedLambda :: Ast
exampleNestedLambda = Lambda ["f", "x"] (Call (Symbol "f") [Symbol "x"])