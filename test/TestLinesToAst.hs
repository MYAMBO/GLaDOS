{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestLinesToAst
-}

module TestLinesToAst (testslinesToAst) where

import Test.Tasty
import Data.Char (ord)
import Test.Tasty.HUnit

import Parsing
import Parser.Data
import Parser.Body
import Parser.Tools
import Parser.Parse
import Parser.Define
import Parser.Constructor

testslinesToAst :: TestTree
testslinesToAst = testGroup "All Parsing Tests"
  [ languageParserTests
  , expressionParserTests
  , bodyParserTests
  ]

-- ================================================================= --
-- ==               TESTS POUR LES PARSEURS DU LANGAGE              == --
-- ================================================================= --

languageParserTests :: TestTree
languageParserTests = testGroup "Language Parsers"
  [ testGroup "astDefine"
      [ testCase "Simple Int32 definition" $
          runParser astDefine "define Int32 x = 10" @?= Just (Define "x" (Var (Int32 10) "x"), "")
      , testCase "Definition with extra spaces" $
          runParser astDefine "define   Double  pi  =  3.14  " @?= Just (Define "pi" (Var (Double 3.14) "pi"), "")
--      , testCase "Missing value" $
--          runParser astDefine "define Int32 y =" @?= Nothing
      , testCase "Invalid type" $
          runParser astDefine "define MyInvalidType z = 1" @?= Just (Define "z" (Var (String "error") "z")," 1")
      ]

  , testGroup "astConstructor"
      [ testCase "Function with one argument" $
          runParser astConstructor "func add<Int32 n> => Int32" @?=
            Just (Define "add" (Lambda [Var (Int32 0) "n"] (Symbol "Int32") (Symbol "body")), "")
      , testCase "Function with multiple arguments" $
          runParser astConstructor "func sub<Int32 a, Int32 b> => Int32" @?=
            Just (Define "sub" (Lambda [Var (Int32 0) "a", Var (Int32 0) "b"] (Symbol "Int32") (Symbol "body")), "")
      , testCase "Function with no arguments" $
          runParser astConstructor "func main<> => Int32" @?=
            Just (Define "main" (Lambda [] (Symbol "Int32") (Symbol "body")), "")
      , testCase "Malformed function (missing arrow)" $
          runParser astConstructor "func error<Int32 a> Int32" @?= Nothing
      ]
  ]

-- ================================================================= --
-- ==                TESTS POUR LES EXPRESSIONS                   == --
-- ================================================================= --

expressionParserTests :: TestTree
expressionParserTests = testGroup "Expression Parsers"
  [ testGroup "parseAtom"
      [ testCase "Integer literal" $
          parseAtom [] [] "123" @?= Right (Literal (Int32 123))
      , testCase "Negative integer literal" $
          parseAtom [] [] "-45" @?= Right (Literal (Int32 (-45)))
      , testCase "Double literal" $
          parseAtom [] [] "3.14" @?= Right (Literal (Double 3.14))
      , testCase "Boolean literal" $
          parseAtom [] [] "True" @?= Right (Literal (Bool True))
      , testCase "Character literal" $
          parseAtom [] [] "'c'" @?= Right (Literal (Int8 (fromIntegral (ord 'c'))))
      , testCase "Unterminated character literal" $
          parseAtom [] [] "'c" @?= Left "Syntax error: Invalid or unterminated character literal: 'c"
      , testCase "Simple variable symbol" $
          parseAtom [] [Var (Int32 0) "x"] "x" @?= Right (Symbol "x")
      , testCase "Undeclared variable" $
          parseAtom [] [] "undeclared" @?= Left "Error: Use of undeclared function or variable 'undeclared'"
      ]

  , testGroup "parseExpression"
      [ testCase "Simple binary operation" $
          parseExpression [] [] "5 + 10" @?= Right (BinOp Add [Literal (Int32 5), Literal (Int32 10)])
      , testCase "Operation with variables" $
          let env = []
              localArgs = [Var (Int32 0) "n"]
          in parseExpression env localArgs "n == 1" @?= Right (BinOp Equal [Symbol "n", Literal (Int32 1)])
      , testCase "Function call without arguments" $
          let env = [Define "myFunc" (Lambda [] (Symbol "Int32") (List []))]
          in parseExpression env [] "myFunc" @?= Right (Call (Symbol "myFunc") [])
      , testCase "Function call with one argument" $
          parseExpression [] [] "fact 5" @?= Right (Call (Symbol "fact") [Literal (Int32 5)])
      , testCase "Incomplete expression" $
          parseExpression [] [] "5 > " @?= Left "Incomplete expression for operator '>' in: \"5 >\""
      ]
  ]

-- ================================================================= --
-- ==                   TESTS POUR LE CORPS DE FONCTION             == --
-- ================================================================= --

bodyParserTests :: TestTree
bodyParserTests = testGroup "Function Body Parsers"
  [ testGroup "buildBodyAsts (via fillBody)"
      [ testCase "Simple single-line body" $
          let env = [Define "main" (Lambda [] (Symbol "Int32") (Symbol "body"))]
              body = ["-> 42"]
              expected = Right [Define "main" (Lambda [] (Symbol "Int32") (If (Literal (Bool True)) (Literal (Int32 42)) (List [])))]
          in fillBody env "main" body @?= expected
      , testCase "If-then structure" $
          let env = [Define "isNeg" (Lambda [Var (Int32 0) "n"] (Symbol "Bool") (Symbol "body"))]
              body = ["n < 0 -> True"]
              expected = Right [Define "isNeg" (Lambda [Var (Int32 0) "n"] (Symbol "Bool") (If (BinOp LessThan [Symbol "n", Literal (Int32 0)]) (Literal (Bool True)) (List [])))]
          in fillBody env "isNeg" body @?= expected
      , testCase "If-then-else structure" $
          let env = [Define "isNeg" (Lambda [Var (Int32 0) "n"] (Symbol "Bool") (Symbol "body"))]
              body = ["n < 0 -> True", "-> False"]
              expectedAst = If (BinOp LessThan [Symbol "n", Literal (Int32 0)])
                               (Literal (Bool True))
                               (If (Literal (Bool True)) (Literal (Bool False)) (List []))
              expected = Right [Define "isNeg" (Lambda [Var (Int32 0) "n"] (Symbol "Bool") expectedAst)]
          in fillBody env "isNeg" body @?= expected
      , testCase "Body with multiple else lines" $
          let env = [Define "main" (Lambda [] (Symbol "Int32") (Symbol "body"))]
              body = ["-> 1", "-> 2"]
              expectedAst = If (Literal (Bool True)) (Literal (Int32 1)) (If (Literal (Bool True)) (Literal (Int32 2)) (List []))
              expected = Right [Define "main" (Lambda [] (Symbol "Int32") expectedAst)]
          in fillBody env "main" body @?= expected
      , testCase "Error on undefined function" $
          fillBody [] "nonExistent" ["-> 1"] @?= Left "Function \"nonExistent\" is not defined."
      ]
  ]