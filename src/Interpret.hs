{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Interpret
-}

module Interpret where

import Control.Applicative ((<|>))
import DataStored
import Tools hiding (evalInt)


evalSymbols :: Env -> String -> [Ast] -> Maybe Ast
evalSymbols env s args = 
    (evalMore env s args)       <|>     (evalLess env s args)     <|>
    (evalMultiply env s args)   <|>     (evalDivide env s args)   <|>
    (evalModulo env s args)     <|>     (evalGreater env s args)  <|>
    (evalSmaller env s args)    <|>     (evalEquality env s args)

evalMore :: Env -> String -> [Ast] -> Maybe Ast
evalMore env "+" args=
    fmap (Atom . sum) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Atom n, _) <- eval e ast
            Just n
evalMore _ _ _ = Nothing

evalLess :: Env -> String -> [Ast] -> Maybe Ast
evalLess env "-" args = do
    fmap (Atom . foldl1 (-)) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Atom n, _) <- eval e ast
            Just n
evalLess _ _ _ = Nothing

evalMultiply :: Env -> String -> [Ast] -> Maybe Ast
evalMultiply env "*" args = do
    fmap (Atom . product) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Atom n, _) <- eval e ast
            Just n
evalMultiply _ _ _ = Nothing

evalDivide :: Env -> String -> [Ast] -> Maybe Ast
evalDivide _ _ args | length args < 2 = Nothing
evalDivide env "div" args =
    fmap (Atom . foldl1 safeDiv) (traverse (evalInt env) args)
  where
    evalInt e ast = do
        (Atom n, _) <- eval e ast
        Just n
evalDivide _ _ _ = Nothing

evalModulo :: Env -> String -> [Ast] -> Maybe Ast
evalModulo _ _ args | length args < 2 = Nothing
evalModulo env "mod" args = 
    fmap (Atom . foldl1 safeMod) (traverse (evalInt env) args)
  where
    evalInt e ast = do
        (Atom n, _) <- eval e ast
        Just n
evalModulo _ _ _ = Nothing

evalGreater :: Env -> String -> [Ast] -> Maybe Ast
evalGreater _ _ args | null args = Nothing
evalGreater env ">" args = do
    list <- (traverse (evalInt env) args)
    Just (ABool (and [x > y | (x, y) <- zip list (drop 1 list)]))
  where
    evalInt e ast = do
        (Atom n, _) <- eval e ast
        Just n
evalGreater _ _ _ = Nothing

evalSmaller :: Env -> String -> [Ast] -> Maybe Ast
evalSmaller _ _ args | null args = Nothing
evalSmaller env "<" args = do
    list <- (traverse (evalInt env) args)
    Just (ABool (and [x < y | (x, y) <- zip list (drop 1 list)]))
  where
    evalInt e ast = do
        (Atom n, _) <- eval e ast
        Just n
evalSmaller _ _ _ = Nothing

evalEquality :: Env -> String -> [Ast] -> Maybe Ast
evalEquality env "eq?" [a, b] = do
    (va, _) <- eval env a
    (vb, _) <- eval env b
    Just (ABool (va == vb))
evalEquality _ _ _ = Nothing

evalAtom :: Env -> Ast -> Maybe AstResult
evalAtom env (Atom n) = Just (Atom n, env)
evalAtom env (Symbol s) = do
    v <- lookupVar s env
    Just (v, env)
evalAtom _ _  = Nothing

evalDefine :: Env -> String -> Ast -> Maybe AstResult
evalDefine env var body =
    Just (body, (var, body) : env)

evalList :: Env -> [Ast] -> Maybe AstResult
evalList env [] = Just (List [], env)
evalList env (Symbol "define" : Symbol var : body : []) = evalDefine env var body
evalList env (Symbol f : args) = do
    val <- evalSymbols env f args
    Just (val, env)
evalList _ _ = Nothing

eval :: Env -> Ast -> Maybe AstResult
eval env (Atom n) = Just (Atom n, env)
eval env (Symbol s) = do
    v <- lookupVar s env
    Just (v, env)
eval env (Define var b) = evalDefine env var b
eval env (List xs) = evalList env xs
eval env (ABool b) = Just (ABool b, env)
eval env (If cond t f) = do
    (ABool c, env1) <- eval env cond
    if c then eval env1 t else eval env1 f
eval env (Call f args) = Nothing
eval env (Lambda p body) = Just (Lambda p body, env)



example1 :: Ast
example1 = List [Symbol "+", Atom 1, Atom 2, Atom 3]

example2 :: Ast
example2 = List [Symbol "-", Symbol "hello", Atom 2, Atom 3]

example3 :: Ast
example3 = List [Symbol "*", Atom 2, Atom 4]

example4 :: Ast
example4 = List [Symbol "div", Atom 6, Atom 2]

example5 :: Ast
example5 = List [Symbol "div", Atom 6, Atom 0]

example6 :: Ast
example6 = List [Symbol "mod", Atom 10, Atom 4]

example7 :: Ast
example7 = List [Symbol "mod", Atom 10, Atom 0]

example8 :: Ast
example8 = List [Symbol ">", Atom 10, Atom 4]

example9 :: Ast
example9 = List [Symbol ">", Atom 0, Atom 10]

example10 :: Ast
example10 = List [Symbol "<", Atom 0, Atom 4]

example11 :: Ast
example11 = List [Symbol "<", Atom 10, Atom 0]

example12 :: Ast
example12 = List [Symbol "eq?", Atom 0, Atom 0]

example13 :: Ast
example13 = List [Symbol "eq?", Atom 10, Atom 0]