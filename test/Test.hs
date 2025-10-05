{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Runners.AntXML (antXMLRunner)
import Test.Tasty.Ingredients.Basic

import System.IO
import Text.Printf

import qualified TestParsing
import qualified TestInterpret
import qualified TestAst
import qualified TestParseSExpr

main :: IO ()
main = defaultMainWithIngredients [antXMLRunner, consoleTestReporter, listingTests] $
  testGroup "Tests"
    [ TestParsing.tests
    , testGroup "AST" [TestAst.tests]
    , testGroup "Interpret" [TestInterpret.tests]
    , testGroup "ParseSExpr" [TestParseSExpr.tests]
    ]
