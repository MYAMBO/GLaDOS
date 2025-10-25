{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Runners.AntXML (antXMLRunner)
import Test.Tasty.Ingredients.Basic

import System.IO
import Text.Printf

main :: IO ()
main = defaultMainWithIngredients [antXMLRunner, consoleTestReporter, listingTests] $
  testGroup "Tests"
    [
    ]
