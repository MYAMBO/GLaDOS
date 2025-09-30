{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- AstPrint
-}

module Ast.AstPrint where

import DataStored

printAst :: Ast -> String
printAst (Atom n) = show n
printAst (Symbol s) = s
printAst (ABool True) = "#t"
printAst (ABool False) = "#f"
printAst (List xs) = "(" ++ unwords (map printAst xs) ++ ")"
printAst ast = show ast
