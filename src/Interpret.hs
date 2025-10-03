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

-------------------------------- All symbols -------------------------------------------------------

evalSymbols :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalSymbols env s args =
    (evalMore env s args)       <|>     (evalLess env s args)     <|>
    (evalMultiply env s args)   <|>     (evalDivide env s args)   <|>
    (evalModulo env s args)     <|>     (evalGreater env s args)  <|>
    (evalSmaller env s args)    <|>     (evalEquality env s args) <|>
    (evalListFunc env s args)   <|>     (evalCons env s args)     <|>
    (evalCar env s args)        <|>     (evalNull env s args)     <|>
    (evalCdr env s args)

evalMore :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalMore env "+" args =
    fmap (Right . Atom . sum) (traverse (evalInt env) args)
  where
    evalInt e ast =
        case eval e ast of
            Right ( Just (Atom n), _) -> Just n
            _ -> Nothing
evalMore _ _ _ = Nothing

evalLess :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalLess _ "-" args | length args < 1 = Just (Left "Exception: (-) not enough argument")
evalLess env "-" args =
    fmap (Right . Atom . foldl1 (-)) (traverse (evalInt env) args)
  where
    evalInt e ast =
        case eval e ast of
            Right ( Just (Atom n), _) -> Just n
            _ -> Nothing
evalLess _ _ _ = Nothing

evalMultiply :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalMultiply env "*" args =
    fmap (Right . Atom . product) (traverse (evalInt env) args)
  where
    evalInt e ast =
        case eval e ast of
            Right ( Just (Atom n), _) -> Just n
            _ -> Nothing
evalMultiply _ _ _ = Nothing

evalDivide :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalDivide _ "div" args | length args < 2 =
    Just (Left "Exception: (div) not enough argument")
evalDivide env "div" args = do
        ints <- traverse (evalInt env) args
        if any (== 0) (tail ints)
            then Just (Left "Exception: division by zero")
            else return $ Right (Atom (foldl1 safeDiv ints))
  where
    evalInt e ast =
        case eval e ast of
            Right ( Just (Atom n), _) -> Just n
            _ -> Nothing
evalDivide _ _ _ = Nothing


evalModulo :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalModulo _ "mod" args | length args < 2 = Just (Left "Exception: (mod) not enough argument")
evalModulo env "mod" args = do
    ints <- traverse (evalInt env) args
    if any (== 0) (tail ints)
        then Just (Left "Exception: modulo by zero")
        else return $ Right (Atom (foldl1 safeMod ints))
  where
    evalInt e ast =
        case eval e ast of
            Right ( Just (Atom n), _) -> Just n
            _ -> Nothing
evalModulo _ _ _ = Nothing

evalGreater :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalGreater _ ">" args | null args = Just (Left "Exception: (>) not enough argument")
evalGreater env ">" args = do
    list <- traverse (evalInt env) args
    Just (Right (ABool (and [x > y | (x, y) <- zip list (drop 1 list)])))
  where
    evalInt e ast =
        case eval e ast of
            Right ( Just (Atom n), _) -> Just n
            _ -> Nothing
evalGreater _ _ _ = Nothing

evalSmaller :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalSmaller _ "<" args | null args = Nothing
evalSmaller env "<" args = do
    list <- traverse (evalInt env) args
    Just (Right (ABool (and [x < y | (x, y) <- zip list (drop 1 list)])))
  where
    evalInt e ast =
        case eval e ast of
            Right ( Just (Atom n), _) -> Just n
            _ -> Nothing
evalSmaller _ _ _ =  Nothing

evalEquality :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalEquality env "eq?" [a, b] = do
    (va, _) <- eitherToMaybe (eval env a)
    (vb, _) <- eitherToMaybe (eval env b)
    Just (Right (ABool (va == vb)))
evalEquality _ "eq?" [] = Just (Left "Exception: (eq?) not enough parameter")
evalEquality _ "eq?" [_] = Just (Left "Exception: (eq?) not enough parameter")
evalEquality _ "eq?" (_:_) = Just (Left "Exception: (eq?) to much parameter")
evalEquality _ _ _ = Nothing

evalListFunc :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalListFunc env "list" args =
    fmap (Right . List) (traverse evalArg args)
  where
    evalArg ast = do
        (res, _) <- eitherToMaybe (eval env ast)
        res
evalListFunc _ _ _ = Nothing

evalCons :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalCons env "cons" [element, listToPrepend] = do
    List xs <- eitherToMaybe (eval env listToPrepend) >>= fst
    x <- eitherToMaybe (eval env element) >>= fst
    return (Right (List (x : xs)))
evalCons _ _ _ = Nothing

evalCar :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalCar env "car" [arg] = do
    List (x:_) <- eitherToMaybe (eval env arg) >>= fst
    return (Right x)
evalCar _ _ _ = Nothing

evalNull :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalNull env "null?" [arg] = do
    (mval, _) <- eitherToMaybe (eval env arg)
    case mval of
        Just (List []) -> Just (Right (ABool True))
        Just (List _)  -> Just (Right (ABool False))
        _              -> Nothing
evalNull _ _ _ = Nothing

evalCdr :: Env -> String -> [Ast] -> Maybe (Either String Ast)
evalCdr env "cdr" [arg] = do
    (Just (List (_:xs)), _) <- eitherToMaybe (eval env arg)
    Just ( Right (List xs))
evalCdr _ _ _ = Nothing

-------------------------------- Define -------------------------------------------------------

evalDefine :: Env -> String -> Ast -> Either String AstResult
evalDefine env name expr = do
    (val, _) <- eval env expr
    case val of
        Just v  -> Right (Nothing, (name, v) : env)
        Nothing -> Left "Exception: invalid syntax"

-------------------------------- Call -------------------------------------------------------

evalUserLambda :: Env -> Ast -> [Ast] -> Either String AstResult
evalUserLambda env (Lambda params body) args = 
    if length params /= length args
        then Left "Exception: wrong number of parameter"
        else do
            evaledArgs <- mapM (\arg -> do
                (mVal, _) <- eval env arg
                case mVal of
                    Just val -> return val
                    Nothing -> Left "Evaluation failed") args
            let localEnv = zip params evaledArgs ++ env
            (res, _) <- eval localEnv body
            Right (res, env)
evalUserLambda _ _ _ = Left "Exception: evalUserLambda"

evalBuiltinCall :: Env -> String -> [Ast] -> Maybe (Either String AstResult)
evalBuiltinCall env s args = do
    case evalSymbols env s args of
        Just (Right result) -> Just (Right (Just result, env))
        Just (Left result) -> Just (Left result)
        _ -> Nothing

evalCall :: Env -> Ast -> [Ast] -> Either String AstResult
evalCall env (Symbol s) args = 
    case evalBuiltinCall env s args of
        Just (Right result) -> Right result
        Just (Left err)     -> Left err
        Nothing ->
            case lookupVar s env of
                Just lam@(Lambda _ _) -> evalUserLambda env lam args
                _ -> Left "Exception: undefined function or invalid call"
evalCall _ _ _ = Left "Exception: invalid call"


-------------------------------- List -------------------------------------------------------

evalLambdaApp :: Env -> [String] -> Ast -> [Ast] -> Either String AstResult
evalLambdaApp env params body args =
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
        Just (Right result) -> Right (Just result, env)
        Just (Left result) -> Left result
        Nothing ->
            case lookupVar s env of
                Just (Lambda params body) -> evalLambdaApp env params body args
                _ -> Left ("Unbound variable: " ++ s)

evalList :: Env -> [Ast] -> Either String AstResult
evalList env [] =  Right (Just (List []), env)
evalList env [x] = eval env x
evalList env (Symbol s : args) = evalSymbolApp env s args
evalList env (Lambda params body : args) = evalLambdaApp env params body args
evalList env (x:xs) = do
    (_, env1) <- eval env x
    evalList env1 xs

-------------------------------- Global eval -------------------------------------------------------

eval :: Env -> Ast -> Either String AstResult
eval env (Atom n)       = Right (Just (Atom n), env)
eval env (Symbol s)     = do
    v <- maybeToEither ("Unbound variable: " ++ s) (lookupVar s env)
    Right (Just v, env)
eval env (Define var b) = evalDefine env var b
eval env (List xs)      = evalList env xs
eval env (ABool b)      = Right (Just (ABool b), env)
eval env (If cond t f)  = do
    (mVal, env1) <- eval env cond
    case mVal of
        Just (ABool c) -> if c then eval env1 t else eval env1 f
        _ -> Left "Exception: invalid condition in if"
eval env (Call f args)  = evalCall env f args
eval env (Lambda p body)= Right (Just (Lambda p body), env)
