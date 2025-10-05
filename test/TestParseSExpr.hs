{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestParseSExpr
-}

module TestParseSExpr (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import ParseSExpr
import Ast.Ast (SExpr(..))
import Parsing (runParser)

tests :: TestTree
tests = testGroup "ParseSExpr tests"
  [ testGroup "parseQuoted"
      [ testCase "parse simple quoted string" $
          runParser parseQuoted "\"hello\"world" @?= Just ("hello", "world")
      , testCase "parse empty quoted string" $
          runParser parseQuoted "\"\"remaining" @?= Just ("", "remaining")
      , testCase "parse quoted string with spaces" $
          runParser parseQuoted "\"hello world\"" @?= Just ("hello world", "")
      , testCase "fail on missing opening quote" $
          runParser parseQuoted "hello\"world" @?= Nothing
      , testCase "fail on missing closing quote" $
          runParser parseQuoted "\"hello world" @?= Nothing
      ]

  , testGroup "parseUnquoted"
      [ testCase "parse simple symbol" $
          runParser parseUnquoted "hello world" @?= Just ("hello", " world")
      , testCase "parse symbol until parenthesis" $
          runParser parseUnquoted "symbol)" @?= Just ("symbol", ")")
      , testCase "parse symbol until space" $
          runParser parseUnquoted "test " @?= Just ("test", " ")
      , testCase "parse empty on space" $
          runParser parseUnquoted " hello" @?= Just ("", " hello")
      ]

  , testGroup "parseAtom"
      [ testCase "parse positive integer atom" $
          runParser parseAtom "42 rest" @?= Just (SAtom 42, " rest")
      , testCase "parse negative integer atom" $
          runParser parseAtom "-15 rest" @?= Just (SAtom (-15), " rest")
      , testCase "parse quoted symbol atom" $
          runParser parseAtom "\"hello\" rest" @?= Just (SSymbol "hello", " rest")
      , testCase "parse unquoted symbol atom" $
          runParser parseAtom "symbol rest" @?= Just (SSymbol "symbol", " rest")
      , testCase "parse boolean true symbol" $
          runParser parseAtom "#t rest" @?= Just (SSymbol "#t", " rest")
      , testCase "parse boolean false symbol" $
          runParser parseAtom "#f rest" @?= Just (SSymbol "#f", " rest")
      ]

  , testGroup "parseElement"
      [ testCase "parse atom element" $
          runParser parseElement "42 rest" @?= Just (SAtom 42, " rest")
      , testCase "parse symbol element" $
          runParser parseElement "hello rest" @?= Just (SSymbol "hello", " rest")
      , testCase "parse list element" $
          runParser parseElement "()" @?= Just (SList [], "")
      , testCase "parse nested list element" $
          runParser parseElement "(42)" @?= Just (SList [SAtom 42], "")
      ]

  , testGroup "parseSExprList"
      [ testCase "parse empty list" $
          runParser parseSExprList ")" @?= Just ([], "")
      , testCase "parse single element list" $
          runParser parseSExprList "42)" @?= Just ([SAtom 42], "")
      , testCase "parse multiple elements with spaces" $
          runParser parseSExprList " 1 2 3 )" @?= Just ([SAtom 1, SAtom 2, SAtom 3], "")
      , testCase "parse mixed elements" $
          runParser parseSExprList "hello 42 \"world\")" @?= Just ([SSymbol "hello", SAtom 42, SSymbol "world"], "")
      , testCase "parse with newlines and tabs" $
          runParser parseSExprList "\n\t42\n\t)" @?= Just ([SAtom 42], "")
      ]

  , testGroup "parseSExprNext"
      [ testCase "parse simple list content" $
          runParser parseSExprNext "( 42 )" @?= Just ([SAtom 42], "")
      , testCase "parse empty list content" $
          runParser parseSExprNext "()" @?= Just ([], "")
      , testCase "parse multiple elements with whitespace" $
          runParser parseSExprNext "( 1 hello \"world\" )" @?= Just ([SAtom 1, SSymbol "hello", SSymbol "world"], "")
      , testCase "fail on missing opening parenthesis" $
          runParser parseSExprNext "42)" @?= Nothing
      ]

  , testGroup "parseSExpr"
      [ testCase "parse atom as SExpr" $
          runParser parseSExpr "42" @?= Just (SAtom 42, "")
      , testCase "parse symbol as SExpr" $
          runParser parseSExpr "hello" @?= Just (SSymbol "hello", "")
      , testCase "parse quoted symbol as SExpr" $
          runParser parseSExpr "\"hello world\"" @?= Just (SSymbol "hello world", "")
      , testCase "parse empty list as SExpr" $
          runParser parseSExpr "()" @?= Just (SList [], "")
      , testCase "parse simple list as SExpr" $
          runParser parseSExpr "(42)" @?= Just (SList [SAtom 42], "")
      , testCase "parse complex list as SExpr" $
          runParser parseSExpr "(+ 1 2)" @?= Just (SList [SSymbol "+", SAtom 1, SAtom 2], "")
      , testCase "parse nested lists as SExpr" $
          runParser parseSExpr "(list (+ 1 2) 3)" @?= Just (SList [SSymbol "list", SList [SSymbol "+", SAtom 1, SAtom 2], SAtom 3], "")
      , testCase "parse with leading whitespace" $
          runParser parseSExpr "   42" @?= Just (SAtom 42, "")
      , testCase "parse boolean symbols" $
          runParser parseSExpr "#t" @?= Just (SSymbol "#t", "")
      , testCase "parse negative numbers" $
          runParser parseSExpr "-42" @?= Just (SAtom (-42), "")
      ]

  , testGroup "real Lisp expressions"
      [ testCase "parse define expression" $
          runParser parseSExpr "(define x 5)" @?= Just (SList [SSymbol "define", SSymbol "x", SAtom 5], "")
      , testCase "parse function call" $
          runParser parseSExpr "(+ 1 2 3)" @?= Just (SList [SSymbol "+", SAtom 1, SAtom 2, SAtom 3], "")
      , testCase "parse lambda expression" $
          runParser parseSExpr "(lambda (x y) (+ x y))" @?= Just (SList [SSymbol "lambda", SList [SSymbol "x", SSymbol "y"], SList [SSymbol "+", SSymbol "x", SSymbol "y"]], "")
      , testCase "parse if expression" $
          runParser parseSExpr "(if #t 1 0)" @?= Just (SList [SSymbol "if", SSymbol "#t", SAtom 1, SAtom 0], "")
      , testCase "parse nested arithmetic" $
          runParser parseSExpr "(* (+ 1 2) (- 5 3))" @?= Just (SList [SSymbol "*", SList [SSymbol "+", SAtom 1, SAtom 2], SList [SSymbol "-", SAtom 5, SAtom 3]], "")
      , testCase "parse function definition" $
          runParser parseSExpr "(define (square x) (* x x))" @?= Just (SList [SSymbol "define", SList [SSymbol "square", SSymbol "x"], SList [SSymbol "*", SSymbol "x", SSymbol "x"]], "")
      ]

  , testGroup "edge cases and error handling"
      [ testCase "parse with extra whitespace" $
          runParser parseSExpr "  \n\t  (  +  1   2  )  " @?= Just (SList [SSymbol "+", SAtom 1, SAtom 2], "  ")
      , testCase "parse string with special characters" $
          runParser parseSExpr "special-symbol!" @?= Just (SSymbol "special-symbol!", "")
      , testCase "parse zero" $
          runParser parseSExpr "0" @?= Just (SAtom 0, "")
      ]
  ]
