{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- DataStored
-}

module DataStored where

data Ast =
      Define String Ast
    | Atome Int
    | Symbole String
    | Liste [Ast]
    deriving (Show)

type Env = [(String, Ast)]
type AstResult = (Ast, Env)

lookupVar :: String -> Env -> Maybe Ast
lookupVar _ [] = Nothing
lookupVar s ((k,v):xs)
    | s == k    = Just v
    | otherwise = lookupVar s xs
