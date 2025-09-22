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
evalInt _ [Atom n] = Just n
evalInt _ _ = Nothing

safeDiv :: Int -> Int -> Int
safeDiv _ 0 = error "Division by zero"
safeDiv x y = div x y

safeMod :: Int -> Int -> Int
safeMod _ 0 = error "Modulo by zero"
safeMod x y = div x y
