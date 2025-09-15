{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- ErrorHandling
-}

module ErrorHandling (Err, throw, runErr) where

type Err a = Either String a

throw :: String -> Err a
throw msg = Left msg

runErr :: Err a -> Either String a
runErr = id
