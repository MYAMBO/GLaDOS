{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Tools
-}

module Tools where

import DataStored

evalInt :: String -> [Ast] -> Maybe Int
evalInt _ [] = Just 0
evalInt _ [Atome n] = Just n
evalInt _ _ = Nothing

safeMod :: Int -> Int -> Int
safeMod _ 0 = error "Modulo by zero"
safeMod x y = div x y