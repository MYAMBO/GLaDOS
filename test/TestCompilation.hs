{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Compilation Tests
-}

module TestCompilation (testCompilation) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

import Compiler (compile)
import DataTypes
import Data.Int (Int32, Int64)

-- Test AST creation for various expressions
simpleMainFunction :: Ast
simpleMainFunction = Define "main" $ 
    Lambda [] (Symbol "Int32") (Literal (Int32 42))

addFunction :: Ast
addFunction = Define "add" $ 
    Lambda [Var (Int32 0) "a", Var (Int32 0) "b"] (Symbol "Int32") 
        (BinOp Add [Symbol "a", Symbol "b"])

mainWithAddCall :: [Ast]
mainWithAddCall = 
    [ addFunction,
      Define "main" $ 
          Lambda [] (Symbol "Int32") 
              (Call (Symbol "add") [Literal (Int32 10), Literal (Int32 20)])
    ]

testCompilation :: TestTree
testCompilation = testGroup "Compilation tests"
  [ testGroup "Simple compilation"
      [ testCase "Simple main function with integer return" $ do
          result <- compile [simpleMainFunction]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> do
              assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
              -- Check for magic number (0x42414B41)
              let magic = BL.take 4 bytecode
              BL.unpack magic @?= [0x42, 0x41, 0x4B, 0x41]

      , testCase "Function with parameters" $ do
          result <- compile mainWithAddCall
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> do
              assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
              -- Check for magic number (0x42414B41)
              let magic = BL.take 4 bytecode
              BL.unpack magic @?= [0x42, 0x41, 0x4B, 0x41]

      , testCase "Compilation fails without main function" $ do
          result <- compile [addFunction]  -- Only add function, no main
          case result of
            Left err -> assertBool "Should fail without main" (err /= "")
            Right _ -> assertFailure "Compilation should have failed without main function"
      ]

  , testGroup "Literal value compilation"
      [ testCase "Integer literal compilation" $ do
          let ast = Define "main" $ Lambda [] (Symbol "Int32") (Literal (Int32 123))
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "Boolean literal compilation" $ do
          let ast = Define "main" $ Lambda [] (Symbol "Bool") (Literal (Bool True))
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "Double literal compilation" $ do
          let ast = Define "main" $ Lambda [] (Symbol "Double") (Literal (Double 3.14))
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
      ]

  , testGroup "Binary operations compilation"
      [ testCase "Add operation compilation" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (BinOp Add [Literal (Int32 5), Literal (Int32 3)])
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "Subtract operation compilation" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (BinOp Subtract [Literal (Int32 10), Literal (Int32 4)])
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "Multiply operation compilation" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (BinOp Multiply [Literal (Int32 6), Literal (Int32 7)])
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "Equality comparison compilation" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "Bool") 
                    (BinOp Equal [Literal (Int32 5), Literal (Int32 5)])
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
      ]

  , testGroup "If-then-else compilation"
      [ testCase "Simple if-then-else" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (If (Literal (Bool True)) 
                        (Literal (Int32 1)) 
                        (Literal (Int32 0)))
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "If-then-else with condition evaluation" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (If (BinOp LessThan [Literal (Int32 1), Literal (Int32 2)]) 
                        (Literal (Int32 100)) 
                        (Literal (Int32 200)))
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
      ]

  , testGroup "Function calls compilation"
      [ testCase "Simple function call without parameters" $ do
          let identityFunc = Define "identity" $ 
                Lambda [Var (Int32 0) "x"] (Symbol "Int32") (Symbol "x")
              mainFunc = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (Call (Symbol "identity") [Literal (Int32 42)])
          result <- compile [identityFunc, mainFunc]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "Recursive function call" $ do
          let factorialFunc = Define "factorial" $ 
                Lambda [Var (Int32 0) "n"] (Symbol "Int32") 
                    (If (BinOp Equal [Symbol "n", Literal (Int32 0)])
                        (Literal (Int32 1))
                        (BinOp Multiply [Symbol "n", 
                            Call (Symbol "factorial") 
                                [BinOp Subtract [Symbol "n", Literal (Int32 1)]]]))
              mainFunc = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (Call (Symbol "factorial") [Literal (Int32 5)])
          result <- compile [factorialFunc, mainFunc]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
      ]

  , testGroup "Variable definitions"
      [ testCase "Simple variable definition" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "Int32") 
                    (List [Define "x" (Literal (Int32 42)), Symbol "x"])
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
      ]

  , testGroup "List operations compilation"
      [ testCase "Empty list creation" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "List") 
                    (LiteralList "Int32" [])
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)

      , testCase "List with elements" $ do
          let ast = Define "main" $ 
                Lambda [] (Symbol "List") 
                    (LiteralList "Int32" [Literal (Int32 1), Literal (Int32 2), Literal (Int32 3)])
          result <- compile [ast]
          case result of
            Left err -> assertFailure $ "Compilation failed: " ++ err
            Right bytecode -> assertBool "Bytecode should not be empty" (BL.length bytecode > 0)
      ]
  ]