{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestParsing
-}

module TestParsing (spec) where

import Control.Applicative (Alternative(..))
import Test.Hspec
import Parsing

spec :: Spec
spec = do
  describe "empty parser" $ do
    it "empty parser fail" $ do
      runParser (empty :: Parser Char) "abc" `shouldBe` Nothing

  describe "parseChar" $ do
    it "invalid parse" $ do
      runParser (parseChar 'a') "bcd" `shouldBe` Nothing
    it "parse char" $ do
      runParser (parseChar 'a') "abc" `shouldBe` Just ('a', "bc")

  describe "parseAnd" $ do
    it "first fail" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'a')) "bcd" `shouldBe` Nothing
    it "second fail" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'a')) "abcd" `shouldBe` Nothing
    it "success" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd" `shouldBe` Just (('a', 'b'), "cd")

  describe "parseInt" $ do
    it "invalid parse" $ do
      runParser parseInt "hello word" `shouldBe` Nothing
    it "parse positive int" $ do
      runParser parseInt "42abc" `shouldBe` Just (42, "abc")
    it "parse negative int" $ do
      runParser parseInt "-15abc" `shouldBe` Just (-15, "abc")
    it "parse int at the end of string" $ do
      runParser parseInt "42" `shouldBe` Just (42, "")

  describe "parseTuple" $ do
    it "invalid parse" $ do
      runParser (parseTuple parseInt) "(42, -84)hello word" `shouldBe` Nothing
    it "parse positif tuple" $ do
      runParser (parseTuple parseInt) "(12,56)hello word" `shouldBe` Just ((12, 56), "hello word")
    it "parse negatif tuple" $ do
      runParser (parseTuple parseInt) "(-412,-5678)hello word" `shouldBe` Just ((-412, -5678), "hello word")

  describe "parseTruple" $ do
    it "invalid parse" $ do
      runParser parseTruple "(42, -84, 789)hello word" `shouldBe` Nothing
    it "parse positif truple" $ do
      runParser parseTruple "(12,56,78)hello word" `shouldBe` Just ((12, 56, 78), "hello word")
    it "parse negatif truple" $ do
      runParser parseTruple "(-412,-5678,-2)hello word" `shouldBe` Just ((-412, -5678, -2), "hello word")

  describe "parseBetween" $ do
    it "invalid parse" $ do
      runParser (parseBetween 1 "[" "]") "[hello word" `shouldBe` Nothing
    it "parse str" $ do
      runParser (parseBetween 1 "\"" "\"") "\"hello word\"blabla" `shouldBe` Just ("hello word", "blabla")
    it "parse str at the end" $ do
      runParser (parseBetween 1 "\"" "\"") "\"hello word\"" `shouldBe` Just ("hello word", "")
    it "parse with different separator" $ do
      runParser (parseBetween 1 "-{|" "|}-") "-{|hello word|}-blabla" `shouldBe` Just ("hello word", "blabla")
    it "parse with different separator at the end" $ do
      runParser (parseBetween 1 "-{|" "|}-") "-{|hello word|}-" `shouldBe` Just ("hello word", "")
    it "parse with finding 2 separators" $ do
      runParser (parseBetween 2 "-{|" "|}-") "-{|hello word|}-|}-|}-" `shouldBe` Just ("hello word|}-", "|}-")
    it "parse with finding 4 separators but only 3 presents" $ do
      runParser (parseBetween 4 "-{|" "|}-") "-{|hello word|}-|}-|}-" `shouldBe` Nothing
    it "parse with finding all separators" $ do
      runParser (parseBetween (-1) "-{|" "|}-") "-{|hello|}-|}-|}-word" `shouldBe` Just ("hello|}-|}-", "word")
    it "failing with -1" $ do
      runParser (parseBetween (-1) "-{|" "|}-") "-{|hello word" `shouldBe` Nothing
    it "failing with -99" $ do
      runParser (parseBetween (-99) "-{|" "|}-") "-{|hello word" `shouldBe` Nothing

  describe "tryParser" $ do
    it "with success" $ do
      runParser (tryMaybe (parseInt)) "-42" `shouldBe` Just (Just (-42), "")
    it "without success" $ do
      runParser (tryMaybe (parseInt)) "f-42" `shouldBe` Just (Nothing, "f-42")
  
  describe "parseAnyCharExcept" $ do
    it "with a char" $ do
      runParser (parseAnyCharExcept "d") "abcdefg" `shouldBe` Just ("abc", "defg")
    it "with many char" $ do
      runParser (parseAnyCharExcept "wfgmpxdc") "abcdefg" `shouldBe` Just ("ab", "cdefg")
    it "without char" $ do
      runParser (parseAnyCharExcept "z") "abcdefg" `shouldBe` Just ("abcdefg", "")

  describe "parseWithoutConsum" $ do
    it "success" $ do
      runParser (parseWithoutConsum (parseString "hello")) "hello word" `shouldBe` Just ("", "hello word")
    it "fail" $ do
      runParser (parseWithoutConsum (parseString "hello")) "not hello word" `shouldBe` Nothing