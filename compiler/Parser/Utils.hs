{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Utils
-}

module Parser.Utils where

import DataTypes
import Data.Char (isAlphaNum)
import Data.List (find)

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` "+-*/%<>=!&|"

isDefinedFunc :: [Ast] -> String -> Bool
isDefinedFunc env name = any (isMatchingFunc name) env
  where isMatchingFunc n (Define defName (Lambda {})) = n == defName; isMatchingFunc _ _ = False

isLocalArg :: [Ast] -> String -> Bool
isLocalArg args name = any (isMatchingArg name) args
  where isMatchingArg n (Var _ varName) = n == varName; isMatchingArg _ _ = False

isGlobalVar :: [Ast] -> String -> Bool
isGlobalVar env name = any (isGlobalVarDef name) env
  where
    isNotFunction (Lambda {}) = False
    isNotFunction _ = True
    isGlobalVarDef n (Define varName val) = n == varName && isNotFunction val
    isGlobalVarDef _ _ = False

getFuncArgs :: [Ast] -> String -> [Ast]
getFuncArgs env funcName =
  case find (isMatchingFunc funcName) env of
    Just (Define _ (Lambda args _ _)) -> args
    _ -> []
  where isMatchingFunc n (Define defName (Lambda {})) = n == defName; isMatchingFunc _ _ = False

