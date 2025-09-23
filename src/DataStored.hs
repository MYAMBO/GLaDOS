{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- DataStored
-}

module DataStored where

data Ast = Atom Int
         | Symbol String
         | List [Ast]
         | Define String Ast
         | ABool Bool
         | If Ast Ast Ast
         | Call Ast [Ast]
         | Lambda [String] Ast
  deriving (Show, Eq)

type Env = [(String, Ast)]
type AstResult = (Maybe Ast, Env)

lookupVar :: String -> Env -> Maybe Ast
lookupVar _ [] = Nothing
lookupVar s ((k,v):xs)
    | s == k    = Just v
    | otherwise = lookupVar s xs
