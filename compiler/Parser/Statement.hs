{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Statements
-}

module Parser.Statement where

import DataTypes
import Parser.Tools
import Parser.Expression (parseExpression)
import Data.List (isPrefixOf)

type ParseResult = Either String Ast

parseDefineOrAssign :: [Ast] -> [Ast] -> String -> ParseResult
parseDefineOrAssign env localArgs s =
  case breakOn "=" s of
    (left, "=", right) -> do
      valueAst <- parseExpression env localArgs (trimLine right)
      case words (trimLine left) of
        [_type, name] -> Right $ Define name valueAst
        [name] -> Right $ Define name valueAst
        _ -> Left $ "Invalid syntax left of '=': \"" ++ trimLine left ++ "\""
    _ -> Left "Not a definition or assignment."

parseStatement :: [Ast] -> [Ast] -> String -> ParseResult
parseStatement env localArgs s =
  let t = trimLine s
  in if "->" `isPrefixOf` t then Left "Unexpected '->' in statement block."
     else case parseDefineOrAssign env localArgs t of
            Right ast -> Right ast
            Left _ -> parseExpression env localArgs t

parseBlock :: [Ast] -> [Ast] -> [String] -> ParseResult
parseBlock _ _ [] = Right (List [])
parseBlock env localArgs (l:ls) | null (trimLine l) = parseBlock env localArgs ls
parseBlock env localArgs (l:ls) = do
    stmt <- parseStatement env localArgs l
    let newArgs = case stmt of Define n _ -> Var (String "") n : localArgs; _ -> localArgs
    restAst <- parseBlock env newArgs ls
    case (stmt, restAst) of
        (s, List []) -> Right s
        (s, List rs) -> Right $ List (s : rs)
        (s, r)       -> Right $ List [s, r]