{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- TestErrorHandling
-}

module TestErrorHandling (spec) where

import Control.Exception (evaluate, ErrorCall(..))
import ErrorHandling
import Test.Hspec

spec :: Spec
spec = do
    describe "throw" $ do
        it "throw error message" $ do
            runErr (throw "Error" :: Err Int) `shouldBe` Left "Error"

    describe "runErr" $ do
        it "runErr with Left" $ do
            runErr (Left "Error" :: Err Int) `shouldBe` Left "Error"
        it "runErr with Right" $ do
            runErr (Right 42 :: Err Int) `shouldBe` Right 42