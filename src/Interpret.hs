{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Interpret
-}

module Interpret where

import Control.Monad (mapM)
import DataStored

evalSymbols :: Env -> String -> [Ast] -> Maybe Ast
evalSymbols env "*" args = evalMultiply env "*" args
evalSymbols env "+" args = evalMore env "+" args
evalSymbols env "-" args = evalLess env "-" args

evalMore :: Env -> String -> [Ast] -> Maybe Ast
evalMore env "+" args = do
    vals <- mapM (fmap fst . eval env) args
    nums <- mapM getInt vals
    Just (Atome (sum nums))
    where
    getInt (Atome n) = Just n
    getInt _ = Nothing
evalMore _ _ _ = Nothing

evalLess :: Env -> String -> [Ast] -> Maybe Ast
evalLess env "-" args = do
    vals <- mapM (fmap fst . eval env) args
    nums <- mapM getInt vals
    case nums of
        (x:xs) -> Just (Atome (foldl (-) x xs))
        []     -> Just (Atome 0)
    where
    getInt (Atome n) = Just n
    getInt _ = Nothing
evalLess _ _ _ = Nothing

evalMultiply :: Env -> String -> [Ast] -> Maybe Ast
evalMultiply env "*" args = do
    vals <- mapM (fmap fst . eval env) args
    nums <- mapM getInt vals
    Just (Atome (product nums))
    where
    getInt (Atome n) = Just n
    getInt _ = Nothing
evalMultiply _ _ _ = Nothing

evalEquality :: Env -> String -> [Ast] -> Maybe Ast
evalEquality env "eq?" [a, b] = do
    if a == b
        then Just(Ast #t)
        else Just(Ast #f)
evalEquality _ _ _ = Nothing

evalAtom :: Env -> Ast -> Maybe Ast
evalAtom _ (Atome n)    = Just (Atome n)
evalAtom env (Symbole s) = lookupVar s env
evalAtom _ _             = Nothing

evalDefine :: Env -> String -> Ast -> Maybe (Ast, Env)
evalDefine env var body =
    Just (body, (var, body) : env)

evalList :: Env -> [Ast] -> Maybe (Ast, Env)
evalList env [] = Just (Liste [], env)
evalList env (Symbole "define" : Symbole var : body : []) = evalDefine env var body
evalList env (Symbole f : args) = do
    val <- evalSymbols env f args
    Just (val, env)
evalList _ _ = Nothing

eval :: Env -> Ast -> Maybe (Ast, Env)
eval env (Atome n)    = Just (Atome n, env)
eval env (Symbole s)  = do
    val <- lookupVar s env
    Just (val, env)
eval env (Define var body) = evalDefine env var body
eval env (Liste xs)   = evalList env xs




example1 :: Ast
example1 = Liste [Symbole "+", Atome 1, Atome 2, Atome 3]

example2 :: Ast
example2 = Liste [Symbole "-", Symbole "hello", Atome 2, Atome 3]

example3 :: Ast
example3 = Liste [Symbole "*", Atome 2, Atome 4]