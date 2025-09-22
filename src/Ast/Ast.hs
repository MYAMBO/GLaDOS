{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Ast
-}

module Ast.Ast where

import Control.Applicative
import DataStored

data SExpr = SAtom Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show, Eq)

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing

getAtom :: SExpr -> Maybe Int
getAtom (SAtom x) = Just x
getAtom _ = Nothing

getSymbols :: [SExpr] -> Maybe [String]
getSymbols = mapM getSymbol

printTree :: SExpr -> Maybe String
printTree (SAtom a) = Just ("an Atom " ++ show a ++ " ")
printTree (SSymbol s) = Just ("a Symbol " ++ show s ++ " ")
printTree (SList []) = Just "an empty list"
printTree (SList l) = (++) <$> ((++) <$> Just "a List [" <*> (concat <$> mapM printTree l)) <*> Just "]"

sexprToAtom :: SExpr -> Maybe Ast
sexprToAtom (SAtom a) = Just $ Atom a
sexprToAtom _ = Nothing

sexprToSymbol :: SExpr -> Maybe Ast
sexprToSymbol (SSymbol s) = Just $ Symbol s
sexprToSymbol _ = Nothing

-- sexprToList :: SExpr -> Maybe Ast
-- sexprToList (SList l) = List <$> mapM sexprToAST l
-- sexprToList _ = Nothing

sexprToDefine :: SExpr -> Maybe Ast
sexprToDefine (SList[SSymbol "define", SSymbol a, b]) = do
    ast <- sexprToAST b
    Just $ Define a ast
sexprToDefine (SList[SSymbol "define", SList (SSymbol name : args), body]) = do
    argsList <- getSymbols args
    bodyAst <- sexprToAST body
    Just $ Define name (Lambda argsList bodyAst)
sexprToDefine _ = Nothing

sexprToBool :: SExpr -> Maybe Ast
sexprToBool (SSymbol "#t") = Just $ ABool True
sexprToBool (SSymbol "#f") = Just $ ABool False
sexprToBool _ = Nothing

sexprToIf :: SExpr -> Maybe Ast
sexprToIf (SList[SSymbol "if", cond, expr1, expr2]) = do
    condAst <- sexprToAST cond
    expr1Ast <- sexprToAST expr1
    expr2Ast <- sexprToAST expr2
    Just $ If condAst expr1Ast expr2Ast
sexprToIf _ = Nothing

sexprToLambda :: SExpr -> Maybe Ast
sexprToLambda (SList[SSymbol "lambda", SList args, body]) = do
    argsList <- getSymbols args
    bodyAst <- sexprToAST body
    Just $ Lambda argsList bodyAst
sexprToLambda _ = Nothing

sexprToCall :: SExpr -> Maybe Ast
sexprToCall (SList (callee : args)) = do
    calleeAst <- sexprToAST callee
    argsAst <- mapM sexprToAST args
    Just $ Call calleeAst argsAst
sexprToCall _ = Nothing

sexprToAST :: SExpr -> Maybe Ast
sexprToAST x =
    (sexprToAtom x)
    <|> (sexprToBool x)
    <|> (sexprToSymbol x)
    <|> (sexprToLambda x)
    <|> (sexprToIf x)
    <|> (sexprToDefine x)
    <|> (sexprToCall x)
