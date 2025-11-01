{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Runners.AntXML (antXMLRunner)
import Test.Tasty.Ingredients.Basic

import System.IO
import Text.Printf

import TestParsing (tests)
import TestLinesToAst (testslinesToAst)
import TestCompilation (testCompilation)

main :: IO ()
main = defaultMainWithIngredients [antXMLRunner, consoleTestReporter, listingTests] $
  testGroup "GLaDOS Tests"
    [ tests,
      testslinesToAst,
      testCompilation
    ]
