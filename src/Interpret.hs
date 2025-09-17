{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Interpret
-}

module Interpret where

import Control.Applicative ((<|>))
import Control.Monad (mapM)
import DataStored
import Tools

evalSymbols :: Env -> String -> [Ast] -> Maybe Ast
evalSymbols env s args = 
    (evalMore env s args) <|> (evalLess env s args) <|> (evalMultiply env s args)

evalMore :: Env -> String -> [Ast] -> Maybe Ast
evalMore env "+" args=
    fmap (Atome . sum) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Atome n, _) <- eval e ast
            Just n
evalMore _ _ _ = Nothing

evalLess :: Env -> String -> [Ast] -> Maybe Ast
evalLess env "-" args = do
    fmap (Atome . foldl1 (-)) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Atome n, _) <- eval e ast
            Just n
evalLess _ _ _ = Nothing

evalMultiply :: Env -> String -> [Ast] -> Maybe Ast
evalMultiply env "*" args = do
    fmap (Atome . product) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Atome n, _) <- eval e ast
            Just n
evalMultiply _ _ _ = Nothing


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