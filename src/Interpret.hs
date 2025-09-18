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
    (evalMore env s args)       <|>     (evalLess env s args)   <|>
    (evalMultiply env s args)   <|>     (evalDivide env s args) <|>
    (evalModulo env s args)

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

evalDivide :: Env -> String -> [Ast] -> Maybe Ast
evalDivide _ _ args | length args < 2 = Nothing
evalDivide env "div" args =
    fmap (Atome . foldl1 safeDiv) (traverse (evalInt env) args)
  where
    evalInt e ast = do
        (Atome n, _) <- eval e ast
        Just n
evalDivide _ _ _ = Nothing

evalModulo :: Env -> String -> [Ast] -> Maybe Ast
evalModulo _ _ args | length args < 2 = Nothing
evalModulo env "div" args =
    fmap (Atome . foldl1 safeMod) (traverse (evalInt env) args)
  where
    evalInt e ast = do
        (Atome n, _) <- eval e ast
        Just n
evalModulo _ _ _ = Nothing

--evalEquality :: Env -> String -> [Ast] -> Maybe Ast
--evalEquality env "eq?" [a, b] = do
--    if (eval a) === (eval b)
--        then Just(Ast True)
--        else Just(Ast False)
--evalEquality _ _ _ = Nothing

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

example4 :: Ast
example4 = Liste [Symbole "div", Atome 6, Atome 2]

example5 :: Ast
example5 = Liste [Symbole "div", Atome 6, Atome 0]

example6 :: Ast
example6 = Liste [Symbole "mod", Atome 10, Atome 4]

example7 :: Ast
example7 = Liste [Symbole "mod", Atome 10, Atome 0]

example12 :: Ast
example12 = Liste [Symbole "eq?", Atome 00, Atome 0]

example13 :: Ast
example13 = Liste [Symbole "eq?", Atome 10, Atome 0]
