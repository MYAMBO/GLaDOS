{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Body
-}

module CFF.Body where

import CFF.Data
import Parsing
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Control.Applicative ((<|>))

parseBodyHeader :: Parser String
parseBodyHeader = do
  _ <- parseString "$"
  _ <- parseMany (parseAnyChar " \t")
  funcName <- parseAnyCharExcept "\n"
  _ <- parseMany (parseAnyChar " \t\n")
  return funcName

buildBodyAsts :: [String] -> [Ast]
buildBodyAsts lines =
  map parseBodyLine lines

parseBodyLine :: String -> Ast
parseBodyLine line =
  case words line of
    ("any":op:a:b:[]) ->
      BinOp (astFindOperation op) [Symbol a, Symbol b]
    (varType:name:value:[]) ->
      Define name (Var (addValueToVar (astFindType varType) value) name)
    _ ->
      Symbol (trimLine line)

trimLine :: String -> String
trimLine s =
  reverse (dropWhile (`elem` " \t") (reverse (dropWhile (`elem` " \t") s)))

fillBody :: [Ast] -> String -> [String] -> [Ast]
fillBody env funcName rawBodyLines =
  let bodyAsts = buildBodyAsts rawBodyLines
  in map (patchFuncBody funcName bodyAsts) env

patchFuncBody :: String -> [Ast] -> Ast -> Ast
patchFuncBody name body (Define n (Lambda args retType (Symbol "body")))
  | n == name = Define n (Lambda args retType (List body))
patchFuncBody _ _ other = other