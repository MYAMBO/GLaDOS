{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Test
-}

import Test.Hspec
import qualified TestParsing
import qualified TestInterpret

main :: IO ()
main = hspec $ do
    TestParsing.spec
    TestInterpret.spec