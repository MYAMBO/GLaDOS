{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Interpret
-}

module Interpret where

import Control.Applicative ((<|>))
import Tools hiding (evalInt)
import DataStored

evalSymbols :: Env -> String -> [Ast] -> Maybe Ast
evalSymbols env s args = 
    (evalMore env s args)       <|>     (evalLess env s args)     <|>
    (evalMultiply env s args)   <|>     (evalDivide env s args)   <|>
    (evalModulo env s args)     <|>     (evalGreater env s args)  <|>
    (evalSmaller env s args)    <|>     (evalEquality env s args) <|>
    (evalListFunc env s args)   <|>     (evalCons env s args)     <|>
    (evalCar env s args)        <|>     (evalNull env s args)

evalMore :: Env -> String -> [Ast] -> Maybe Ast
evalMore env "+" args =
    fmap (Atom . sum) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Just (Atom n), _) <- eval e ast
            Just n
evalMore _ _ _ = Nothing

evalNull :: Env -> String -> [Ast] -> Maybe Ast
evalNull env "null?" [arg] = do
    lst <- eval env arg >>= fst
    return $ case lst of
        List [] -> ABool True
        _       -> ABool False
evalNull _ _ _ = Nothing

evalCar :: Env -> String -> [Ast] -> Maybe Ast
evalCar env "car" [arg] = do
    List (x:_) <- eval env arg >>= fst
    return x
evalCar _ _ _ = Nothing

evalCons :: Env -> String -> [Ast] -> Maybe Ast
evalCons env "cons" [element, listToPrepend] = do
    List xs <- eval env listToPrepend >>= fst
    x <- eval env element >>= fst
    return $ List (x : xs)
evalCons _ _ _ = Nothing

evalListFunc :: Env -> String -> [Ast] -> Maybe Ast
evalListFunc env "list" args = fmap List (traverse evalArg args)
  where
    evalArg :: Ast -> Maybe Ast
    evalArg ast = do
        (res, _) <- eval env ast
        res
evalListFunc _ _ _ = Nothing

evalLess :: Env -> String -> [Ast] -> Maybe Ast
evalLess env "-" args = do
    fmap (Atom . foldl1 (-)) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Just (Atom n), _) <- eval e ast
            Just n
evalLess _ _ _ = Nothing

evalMultiply :: Env -> String -> [Ast] -> Maybe Ast
evalMultiply env "*" args = do
    fmap (Atom . product) (traverse (evalInt env) args)
    where
        evalInt e ast = do
            (Just (Atom n), _) <- eval e ast
            Just n
evalMultiply _ _ _ = Nothing

evalDivide :: Env -> String -> [Ast] -> Maybe Ast
evalDivide _ _ args | length args < 2 = Nothing
evalDivide env "div" args = do
    ints <- traverse (evalInt env) args
    return $ Atom (foldl1 safeDiv ints)
  where
    evalInt e ast = do
        (Just (Atom n), _) <- eval e ast
        Just n
evalDivide _ _ _ = Nothing

evalModulo :: Env -> String -> [Ast] -> Maybe Ast
evalModulo _ _ args | length args < 2 = Nothing
evalModulo env "mod" args = do
    ints <- traverse (evalInt env) args
    return $ Atom (foldl1 safeMod ints)
  where
    evalInt e ast = do
        (Just (Atom n), _) <- eval e ast
        Just n
evalModulo _ _ _ = Nothing


evalGreater :: Env -> String -> [Ast] -> Maybe Ast
evalGreater _ _ args | null args = Nothing
evalGreater env ">" args = do
    list <- (traverse (evalInt env) args)
    Just (ABool (and [x > y | (x, y) <- zip list (drop 1 list)]))
  where
    evalInt e ast = do
        (Just (Atom n), _) <- eval e ast
        Just n
evalGreater _ _ _ = Nothing

evalSmaller :: Env -> String -> [Ast] -> Maybe Ast
evalSmaller _ _ args | null args = Nothing
evalSmaller env "<" args = do
    list <- (traverse (evalInt env) args)
    Just (ABool (and [x < y | (x, y) <- zip list (drop 1 list)]))
  where
    evalInt e ast = do
        (Just (Atom n), _) <- eval e ast
        Just n
evalSmaller _ _ _ = Nothing

evalEquality :: Env -> String -> [Ast] -> Maybe Ast
evalEquality env "eq?" [a, b] = do
    (va, _) <- eval env a
    (vb, _) <- eval env b
    Just (ABool (va == vb))
evalEquality _ _ _ = Nothing

evalAtom :: Env -> Ast -> Maybe AstResult
evalAtom env (Atom n) = Just (Just (Atom n), env)
evalAtom env (Symbol s) = do
    v <- lookupVar s env
    Just (Just (v), env)
evalAtom _ _  = Nothing

evalDefine :: Env -> String -> Ast -> Maybe AstResult
evalDefine env var body =
    Just (Nothing, (var, body) : env)

evalArgs :: Env -> [String] -> [Ast] -> Maybe AstResult
evalArgs env params args = do
    if length params /= length args
        then Nothing
        else do
            _ <- mapM (\arg -> do
                (mVal, _) <- eval env arg
                case mVal of
                    Just val -> return val
                    Nothing -> fail "Evaluation failed") args
            Just (Just (Symbol "ok"), env)

evalBuiltinCall :: Env -> String -> [Ast] -> Maybe AstResult
evalBuiltinCall env s args = 
    case evalSymbols env s args of
        Just result -> Just (Just result, env)
        Nothing     -> Nothing

evalUserLambda :: Env -> Ast -> [Ast] -> Maybe AstResult
evalUserLambda env (Lambda params body) args
    | length params /= length args = Nothing
    | otherwise = do
        evaledArgs <- mapM (\arg -> do
            (mVal, _) <- eval env arg
            case mVal of
                Just val -> return val
                Nothing -> fail "Evaluation failed") args
        let localEnv = zip params evaledArgs ++ env
        (res, _) <- eval localEnv body
        return (res, env)
evalUserLambda _ _ _ = Nothing

evalCall :: Env -> Ast -> [Ast] -> Maybe AstResult
evalCall env (Symbol s) args =
    case evalBuiltinCall env s args of
        Just result -> Just result
        Nothing -> do
            func <- lookupVar s env
            case func of
                lam@(Lambda _ _) -> evalUserLambda env lam args
                _ -> Nothing
evalCall _ _ _ = Nothing

evalLambdaApp :: Env -> [String] -> Ast -> [Ast] -> Maybe AstResult
evalLambdaApp env params body args = do
    if length params /= length args
        then Nothing
        else do
            evaledArgs <- mapM (\arg -> do
                (mVal, _) <- eval env arg
                case mVal of
                    Just val -> return val
                    Nothing -> fail "Evaluation failed") args
            let localEnv = zip params evaledArgs ++ env
            (res, _) <- eval localEnv body
            return (res, env)

evalSymbolApp :: Env -> String -> [Ast] -> Maybe AstResult
evalSymbolApp env s args = do
    case evalSymbols env s args of
        Just result -> Just (Just (result), env)
        Nothing -> do
            func <- lookupVar s env
            case func of
                Lambda params body -> evalLambdaApp env params body args
                _ -> Nothing

evalList :: Env -> [Ast] -> Maybe AstResult
evalList env [] = Just (Just (List []), env)
evalList env [x] = eval env x
evalList env (Symbol s : args) = evalSymbolApp env s args
evalList env (Lambda params body : args) = evalLambdaApp env params body args
evalList env (x:xs) = do
    (_, env1) <- eval env x
    evalList env1 xs

eval :: Env -> Ast -> Maybe AstResult
eval env (Atom n)     = Just (Just (Atom n), env)
eval env (Symbol s)   = do
    v <- lookupVar s env
    Just (Just v, env)
eval env (Define var b) = evalDefine env var b
eval env (List xs)    = evalList env xs
eval env (ABool b)    = Just (Just (ABool b), env)
eval env (If cond t f) = do
    (Just (ABool c), env1) <- eval env cond
    if c then eval env1 t else eval env1 f
eval env (Call f args) = evalCall env f args
eval env (Lambda p body) = Just (Just (Lambda p body), env)


