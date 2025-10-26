-- test/TestParsing.hs
module TestParsing (tests) where

import Control.Applicative (Alternative(..))
import Test.Tasty
import Test.Tasty.HUnit
import Parsing

tests :: TestTree
tests = testGroup "Parsing tests"
  [ testGroup "empty parser"
      [ testCase "empty parser fail" $
          runParser (empty :: Parser Char) "abc" @?= Nothing
      ]

  , testGroup "parseChar"
      [ testCase "invalid parse" $
          runParser (parseChar 'a') "bcd" @?= Nothing
      , testCase "parse char" $
          runParser (parseChar 'a') "abc" @?= Just ('a', "bc")
      ]

  , testGroup "parseAnd"
      [ testCase "first fail" $
          runParser (parseAnd (parseChar 'a') (parseChar 'a')) "bcd" @?= Nothing
      , testCase "second fail" $
          runParser (parseAnd (parseChar 'a') (parseChar 'a')) "abcd" @?= Nothing
      , testCase "success" $
          runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd" @?= Just (('a','b'), "cd")
      ]

  , testGroup "parseInt"
      [ testCase "invalid parse" $
          runParser parseInt "hello word" @?= Nothing
      , testCase "parse positive int" $
          runParser parseInt "42abc" @?= Just (42, "abc")
      , testCase "parse negative int" $
          runParser parseInt "-15abc" @?= Just (-15, "abc")
      , testCase "parse int at the end of string" $
          runParser parseInt "42" @?= Just (42, "")
      ]

  , testGroup "parseTuple"
      [ testCase "invalid parse" $
          runParser (parseTuple parseInt) "(42, -84)hello word" @?= Nothing
      , testCase "parse positif tuple" $
          runParser (parseTuple parseInt) "(12,56)hello word" @?= Just ((12,56), "hello word")
      , testCase "parse negatif tuple" $
          runParser (parseTuple parseInt) "(-412,-5678)hello word" @?= Just ((-412,-5678), "hello word")
      ]

  , testGroup "parseTruple"
      [ testCase "invalid parse" $
          runParser parseTruple "(42, -84, 789)hello word" @?= Nothing
      , testCase "parse positif truple" $
          runParser parseTruple "(12,56,78)hello word" @?= Just ((12,56,78), "hello word")
      , testCase "parse negatif truple" $
          runParser parseTruple "(-412,-5678,-2)hello word" @?= Just ((-412,-5678,-2), "hello word")
      ]

  , testGroup "parseBetween"
      [ testCase "invalid parse" $
          runParser (parseBetween 1 "[" "]") "[hello word" @?= Nothing
      , testCase "parse str" $
          runParser (parseBetween 1 "\"" "\"") "\"hello word\"blabla" @?= Just ("hello word", "blabla")
      , testCase "parse str at the end" $
          runParser (parseBetween 1 "\"" "\"") "\"hello word\"" @?= Just ("hello word", "")
      , testCase "parse with different separator" $
          runParser (parseBetween 1 "-{|" "|}-") "-{|hello word|}-blabla" @?= Just ("hello word", "blabla")
      , testCase "parse with different separator at the end" $
          runParser (parseBetween 1 "-{|" "|}-") "-{|hello word|}-" @?= Just ("hello word", "")
      , testCase "parse with finding 2 separators" $
          runParser (parseBetween 2 "-{|" "|}-") "-{|hello word|}-|}-|}-" @?= Just ("hello word|}-", "|}-")
      , testCase "parse with finding 4 separators but only 3 presents" $
          runParser (parseBetween 4 "-{|" "|}-") "-{|hello word|}-|}-|}-" @?= Nothing
      , testCase "parse with finding all separators" $
          runParser (parseBetween (-1) "-{|" "|}-") "-{|hello|}-|}-|}-word" @?= Just ("hello|}-|}-", "word")
      , testCase "failing with -1" $
          runParser (parseBetween (-1) "-{|" "|}-") "-{|hello word" @?= Nothing
      , testCase "failing with -99" $
          runParser (parseBetween (-99) "-{|" "|}-") "-{|hello word" @?= Nothing
      ]

  , testGroup "tryParser"
      [ testCase "with success" $
          runParser (tryMaybe parseInt) "-42" @?= Just (Just (-42), "")
      , testCase "without success" $
          runParser (tryMaybe parseInt) "f-42" @?= Just (Nothing, "f-42")
      ]

  , testGroup "parseAnyCharExcept"
      [ testCase "with a char" $
          runParser (parseAnyCharExcept "d") "abcdefg" @?= Just ("abc", "defg")
      , testCase "with many char" $
          runParser (parseAnyCharExcept "wfgmpxdc") "abcdefg" @?= Just ("ab", "cdefg")
      , testCase "without char" $
          runParser (parseAnyCharExcept "z") "abcdefg" @?= Just ("abcdefg", "")
      ]

  , testGroup "parseWithoutConsum"
      [ testCase "success" $
          runParser (parseWithoutConsum (parseString "hello")) "hello word" @?= Just ("", "hello word")
      , testCase "fail" $
          runParser (parseWithoutConsum (parseString "hello")) "not hello word" @?= Nothing
      ]

  , testGroup "parseWhileOneOf"
      [ testCase "parse until func found" $
          runParser (parseWhileOneOf ["int32", "func"]) "   \n   \t func ... int32" @?= Just ("   \n   \t ", "func ... int32")
      , testCase "parse until int32 found" $
          runParser (parseWhileOneOf ["int32", "func"]) "hello world int32 rest" @?= Just ("hello world ", "int32 rest")
      , testCase "parse until first target found" $
          runParser (parseWhileOneOf ["def", "abc"]) "xyz abc def" @?= Just ("xyz ", "abc def")
      , testCase "no target found, parse entire string" $
          runParser (parseWhileOneOf ["missing"]) "hello world" @?= Nothing
      , testCase "target at beginning" $
          runParser (parseWhileOneOf ["hello"]) "hello world" @?= Just ("", "hello world")
      , testCase "empty targets list" $
          runParser (parseWhileOneOf []) "hello world" @?= Nothing
      ]
  ]
