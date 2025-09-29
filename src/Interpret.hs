{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Interpret
-}

module Interpret where

import Control.Applicative ((<|>))
import Tools hiding (evalInt)
import Data.Either()
import DataStored


evalSymbols :: Env -> String -> [Ast] -> Maybe Ast
evalSymbols env s args = 
    (evalMore env s args)       <|>     (evalLess env s args)     <|>
    (evalMultiply env s args)   <|>     (evalDivide env s args)   <|>
    (evalModulo env s args)     <|>     (evalGreater env s args)  <|>
    (evalSmaller env s args)    <|>     (evalEquality env s args)

evalMore :: Env -> String -> [Ast] -> Maybe Ast
evalMore env "+" args =
    fmap (Atom . sum) (traverse (evalInt env) args)
    where
        evalInt e ast =
            case eval e ast of
                Right ( Just (Atom n), _) -> Just n
                _ -> Nothing
evalMore _ _ _ = Nothing

evalLess :: Env -> String -> [Ast] -> Maybe Ast
evalLess env "-" args = do
    fmap (Atom . foldl1 (-)) (traverse (evalInt env) args)
    where
        evalInt e ast =
            case eval e ast of
                Right ( Just (Atom n), _) -> Just n
                _ -> Nothing
evalLess _ _ _ = Nothing

evalMultiply :: Env -> String -> [Ast] -> Maybe Ast
evalMultiply env "*" args = do
    fmap (Atom . product) (traverse (evalInt env) args)
    where
        evalInt e ast =
            case eval e ast of
                Right ( Just (Atom n), _) -> Just n
                _ -> Nothing
evalMultiply _ _ _ = Nothing

evalDivide :: Env -> String -> [Ast] -> Maybe Ast
evalDivide _ _ args | length args < 2 = Nothing
evalDivide env "div" args = do
    ints <- traverse (evalInt env) args
    return $ Atom (foldl1 safeDiv ints)
  where
    evalInt e ast =
            case eval e ast of
                Right ( Just (Atom n), _) -> Just n
                _ -> Nothing
evalDivide _ _ _ = Nothing

evalModulo :: Env -> String -> [Ast] -> Maybe Ast
evalModulo _ _ args | length args < 2 = Nothing
evalModulo env "mod" args = do
    ints <- traverse (evalInt env) args
    return $ Atom (foldl1 safeMod ints)
  where
    evalInt e ast =
            case eval e ast of
                Right ( Just (Atom n), _) -> Just n
                _ -> Nothing
evalModulo _ _ _ = Nothing


evalGreater :: Env -> String -> [Ast] -> Maybe Ast
evalGreater _ _ args | null args = Nothing
evalGreater env ">" args = do
    list <- (traverse (evalInt env) args)
    Just (ABool (and [x > y | (x, y) <- zip list (drop 1 list)]))
  where
    evalInt e ast =
            case eval e ast of
                Right ( Just (Atom n), _) -> Just n
                _ -> Nothing
evalGreater _ _ _ = Nothing

evalSmaller :: Env -> String -> [Ast] -> Maybe Ast
evalSmaller _ _ args | null args = Nothing
evalSmaller env "<" args = do
    list <- (traverse (evalInt env) args)
    Just (ABool (and [x < y | (x, y) <- zip list (drop 1 list)]))
  where
    evalInt e ast =
            case eval e ast of
                Right ( Just (Atom n), _) -> Just n
                _ -> Nothing
evalSmaller _ _ _ = Nothing

evalEquality :: Env -> String -> [Ast] -> Maybe Ast
evalEquality env "eq?" [a, b] = do
        (va, _) <- eitherToMaybe (eval env a)
        (vb, _) <- eitherToMaybe (eval env b)
        Just (ABool (va == vb))
    where
        eitherToMaybe (Left _)  = Nothing
        eitherToMaybe (Right x) = Just x
evalEquality _ _ _ = Nothing

evalDefine :: Env -> String -> Ast -> Either String AstResult
evalDefine env name expr = do
    (val, _) <- eval env expr
    case val of
        Just v  -> Right (Nothing, (name, v) : env)
        Nothing -> Left "Exception: invalid syntax"

evalBuiltinCall :: Env -> String -> [Ast] -> Either String AstResult
evalBuiltinCall env s args = do
    case evalSymbols env s args of
        Just result -> Right (Just result, env)
        Nothing     -> Left "Exception: unknown Symbol"

evalUserLambda :: Env -> Ast -> [Ast] -> Either String AstResult
evalUserLambda env (Lambda params body) args
    | length params /= length args = Left "Exception: wrong number of parameter"
    | otherwise = do
        evaledArgs <- mapM (\arg -> do
            (mVal, _) <- eval env arg
            case mVal of
                Just val -> return val
                Nothing -> Left "Evaluation failed") args
        let localEnv = zip params evaledArgs ++ env
        (res, _) <- eval localEnv body
        Right (res, env)
evalUserLambda _ _ _ = Left "Exception: evalUserLambda"

evalCall :: Env -> Ast -> [Ast] -> Either String AstResult
evalCall env (Symbol s) args =
    case evalBuiltinCall env s args of
        Right result -> Right result
        Left _ -> case lookupVar s env of
            Just lam@(Lambda _ _) -> evalUserLambda env lam args
            _ -> Left "Exception: evalCall 1"
evalCall _ _ _ = Left "Exception: EvalCall 2"

evalLambdaApp :: Env -> [String] -> Ast -> [Ast] -> Either String AstResult
evalLambdaApp env params body args = do
    if length params /= length args
        then Left "Exception: wong number of parameter"
        else do
            evaledArgs <- mapM (\arg -> do
                (mVal, _) <- eval env arg
                case mVal of
                    Just val -> return val
                    Nothing -> Left "Evaluation failed") args
            let localEnv = zip params evaledArgs ++ env
            (res, _) <- eval localEnv body
            Right (res, env)

evalSymbolApp :: Env -> String -> [Ast] -> Either String AstResult
evalSymbolApp env s args = do
    case evalSymbols env s args of
        Just result -> Right (Just (result), env)
        Nothing ->
            case lookupVar s env of
                Just (Lambda params body) -> evalLambdaApp env params body args
                _ -> Left "Exception: evalSymbolApp"

evalList :: Env -> [Ast] -> Either String AstResult
evalList env [] = Right (Just (List []), env)
evalList env [x] = eval env x
evalList env (Symbol s : args) = evalSymbolApp env s args
evalList env (Lambda params body : args) = evalLambdaApp env params body args
evalList env (x:xs) = do
    (_, env1) <- eval env x
    evalList env1 xs

eval :: Env -> Ast -> Either String AstResult
eval env (Atom n)       = Right (Just (Atom n), env)
eval env (Symbol s)     = do
    v <- maybeToEither ("unbound variable: " ++ s) (lookupVar s env)
    Right (Just v, env)
eval env (Define var b) = evalDefine env var b
eval env (List xs)      = evalList env xs
eval env (ABool b)      = Right (Just (ABool b), env)
eval env (If cond t f)  = do
    (mVal, env1) <- eval env cond
    case mVal of
        Just (ABool c) ->
            if c then eval env1 t else eval env1 f
        _ -> Left "Exception: invalid condition in if"
eval env (Call f args)  = evalCall env f args
eval env (Lambda p body)= Right (Just (Lambda p body), env)

