-- Fichier: test/TestLinesToAst.hs
module TestLinesToAst (testslinesToAst) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Char (ord)

-- Imports corrigés pour la nouvelle structure
import DataTypes
import Parser.Expression (parseExpression)
import Parser.Body (fillBody)
-- Pas besoin d'importer parseAtom directement si on teste via parseExpression

-- Type alias pour la clarté des tests
type Env = [Ast]
type LocalArgs = [Ast]

-- Helper pour rendre les tests sur `Either` plus faciles à lire
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

testslinesToAst :: TestTree
testslinesToAst = testGroup "High-Level Parsing Tests"
  [ expressionParserTests
  , bodyParserTests
  , scopeAndStatementTests
  ]

-- ================================================================= --
-- ==          TESTS POUR L'ANALYSE D'EXPRESSION COMPLÈTE           == --
-- ================================================================= --

expressionParserTests :: TestTree
expressionParserTests = testGroup "Expression Parsing"
  [ testGroup "Literals and Simple Atoms"
      [ testCase "Parses an integer" $
          parseExpression [] [] "123" @?= Right (Literal (Int32 123))
      , testCase "Parses a negative integer literal" $
          parseExpression [] [] "-45" @?= Right (BinOp Neg [Literal (Int32 45)])
      , testCase "Parses a double literal" $
          parseExpression [] [] "3.14" @?= Right (Literal (Double 3.14))
      , testCase "Parses a boolean" $
          parseExpression [] [] "True" @?= Right (Literal (Bool True))
      , testCase "Parses a character literal" $
          parseExpression [] [] "'c'" @?= Right (Literal (Int8 (fromIntegral (ord 'c'))))
      , testCase "Rejects an unterminated character literal" $
          assertBool "Should fail on unterminated char" (isLeft (parseExpression [] [] "'c"))
      , testCase "Parses a simple variable as a Symbol" $
          parseExpression [] [Var (Int32 0) "x"] "x" @?= Right (Symbol "x")
      ]

  , testGroup "Operator Precedence"
      [ testCase "Addition and Multiplication" $
          let ast = parseExpression [] [] "2 + 3 * 4"
              expected = Right (BinOp Add [Literal (Int32 2), BinOp Multiply [Literal (Int32 3), Literal (Int32 4)]])
          in ast @?= expected
      , testCase "Multiplication and Addition" $
          let ast = parseExpression [] [] "2 * 3 + 4"
              expected = Right (BinOp Add [BinOp Multiply [Literal (Int32 2), Literal (Int32 3)], Literal (Int32 4)])
          in ast @?= expected
      , testCase "Mixed operators with parentheses" $
          let ast = parseExpression [] [] "(2 + 3) * 4"
              expected = Right (BinOp Multiply [BinOp Add [Literal (Int32 2), Literal (Int32 3)], Literal (Int32 4)])
          in ast @?= expected
      , testCase "Logical and Comparison operators" $
          let ast = parseExpression [] [] "a > b && b > c"
              expected = Right (BinOp And [BinOp GreaterThan [Symbol "a", Symbol "b"], BinOp GreaterThan [Symbol "b", Symbol "c"]])
          in ast @?= expected
      ]

  , testGroup "Unary Operators"
      [ testCase "Simple arithmetic negation" $
          parseExpression [] [] "-5" @?= Right (BinOp Neg [Literal (Int32 5)])
      , testCase "Negation of a variable" $
          parseExpression [] [] "-x" @?= Right (BinOp Neg [Symbol "x"])
      , testCase "Negation of a parenthesized expression" $
          parseExpression [] [] "-(x + y)" @?= Right (BinOp Neg [BinOp Add [Symbol "x", Symbol "y"]])
      , testCase "Logical NOT" $
          parseExpression [] [] "!True" @?= Right (BinOp Not [Literal (Bool True)])
      , testCase "Double negation" $
          parseExpression [] [] "--5" @?= Right (BinOp Neg [BinOp Neg [Literal (Int32 5)]])
      , testCase "Negation with binary operator" $
          let ast = parseExpression [] [] "10 * -2"
              expected = Right (BinOp Multiply [Literal (Int32 10),BinOp Neg [Literal (Int32 2)]])
          in ast @?= expected
      ]

  , testGroup "Function Calls"
      [ testCase "Call with no arguments" $
            let env = [Define "myFunc" (Lambda [] (Symbol "Int32") (List []))]
            in parseExpression env [] "myFunc" @?= Right (Call (Symbol "myFunc") [])
      , testCase "Call with one simple argument" $
            let env = [Define "fact" (Lambda [Var (Int32 0) "n"] (Symbol "Int32") (Symbol "body"))]
            in parseExpression env [] "fact 5" @?= Right (Call (Symbol "fact") [Literal (Int32 5)])
      , testCase "Call with multiple arguments" $
            let env = [Define "add" (Lambda [Var (Int32 0) "a", Var (Int32 0) "b"] (Symbol "Int32") (List []))]
            in parseExpression env [] "add 5 10" @?= Right (Call (Symbol "add") [Literal (Int32 5), Literal (Int32 10)])
      , testCase "Call with parenthesized expression argument" $
            let env = [Define "fact" (Lambda [Var (Int32 0) "n"] (Symbol "Int32") (Symbol "body"))]
            in parseExpression env [Var (Int32 0) "n"] "fact (n - 1)" @?= Right (Call (Symbol "fact") [BinOp Subtract [Symbol "n", Literal (Int32 1)]])
      ]
  ]

-- ================================================================= --
-- ==       TESTS POUR LES CORPS DE FONCTION & IF-THEN-ELSE         == --
-- ================================================================= --

bodyParserTests :: TestTree
bodyParserTests = testGroup "Function Body Structure"
  [ testCase "If-then-else structure" $
      let env = [Define "isNeg" (Lambda [Var (Int32 0) "n"] (Symbol "Bool") (Symbol "body"))]
          body = ["n < 0 -> True", "-> False"]
          expectedAst = If (BinOp LessThan [Symbol "n", Literal (Int32 0)])
                           (Literal (Bool True))
                           (Literal (Bool False))
          expected = Right [Define "isNeg" (Lambda [Var (Int32 0) "n"] (Symbol "Bool") expectedAst)]
      in fillBody env "isNeg" body @?= expected
  , testCase "Rejects body without final 'else' clause" $
      let env = [Define "isNeg" (Lambda [Var (Int32 0) "n"] (Symbol "Bool") (Symbol "body"))]
          body = ["n < 0 -> True"]
      in assertBool "Should fail if 'else' is missing" (isLeft (fillBody env "isNeg" body))
  ]

-- ================================================================= --
-- ==          TESTS POUR LES STATEMENTS & SCOPE LOCAL              == --
-- ================================================================= --

scopeAndStatementTests :: TestTree
scopeAndStatementTests = testGroup "Statements and Local Scope"
  [ testCase "Parses multi-line block with definition and use" $
      let env = [Define "main" (Lambda [] (Symbol "Int32") (Symbol "body"))]
          body = ["-> Int32 x = 10", "   x"]
          expectedBody = List [Define "x" (Literal (Int32 10)), Symbol "x"]
          expected = Right [Define "main" (Lambda [] (Symbol "Int32") expectedBody)]
      in fillBody env "main" body @?= expected

  , testCase "Parses assignment to existing variable" $
      let env = [Define "main" (Lambda [Var (Int32 0) "x"] (Symbol "Int32") (Symbol "body"))]
          body = ["-> x = x + 1", "   x"]
          expectedBody = List [Define "x" (BinOp Add [Symbol "x", Literal (Int32 1)]), Symbol "x"]
          expected = Right [Define "main" (Lambda [Var (Int32 0) "x"] (Symbol "Int32") expectedBody)]
      in fillBody env "main" body @?= expected

  , testCase "Local scope is correctly propagated" $
      let env = [Define "main" (Lambda [] (Symbol "Int32") (Symbol "body"))]
          body = ["-> Int32 x = 10", "   Int32 y = x + 5", "   y"]
          expectedBody = List [Define "x" (Literal (Int32 10)), Define "y" (BinOp Add [Symbol "x", Literal (Int32 5)]), Symbol "y"]
          expected = Right [Define "main" (Lambda [] (Symbol "Int32") expectedBody)]
      in fillBody env "main" body @?= expected
  ]