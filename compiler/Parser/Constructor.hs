{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Constructor
-}

module Parser.Constructor where

import Parser.Define
import Parsing.Parsing
import DataTypes
import Parser.Body
import Parser.Tools
import Debug.Trace (trace)

type Env = [Ast]

parseAllLines :: [String] -> Env -> Env
parseAllLines [] env = env
parseAllLines (line:ls) env
  | "define" `elem` words line =
    case toEither (runParser astDefine line) of
      Right ast -> parseAllLines ls (env ++ [ast])
      Left err -> trace ("Error in define: " ++ err) $ parseAllLines ls env
  | "func" `elem` words line =
    case toEither (runParser astConstructor line) of
      Right ast -> parseAllLines ls (env ++ [ast])
      Left err -> trace ("Error in func: " ++ err) $ parseAllLines ls env
  | "$" `elem` words line =
    case runParser parseBodyHeader line of
      Just (funcName, _) ->
        let (bodyLines, remainingLines) = collectBodyLines ls
            updatedEnv = fillBody env funcName bodyLines
        in parseAllLines remainingLines updatedEnv
      Nothing -> trace ("Failed to parse body header: " ++ line) $ parseAllLines ls env
  | otherwise = trace ("Unknown line format: " ++ line) $ parseAllLines ls env

collectBodyLines :: [String] -> ([String], [String])
collectBodyLines [] = ([], [])
collectBodyLines (l:ls)
  | isIndented l = let (body, rest) = collectBodyLines ls in (trimLine l : body, rest)
  | otherwise = ([], l:ls)
  where
    isIndented s = case s of
      (c:_) -> c == ' ' || c == '\t'
      _ -> False

astArgumentsParser :: Parser [String]
astArgumentsParser = do
  args <- parseWhile (-1) ">"
  let argList = map trimLine (splitArgs args)
  return argList
  where
    splitArgs :: String -> [String]
    splitArgs [] = []
    splitArgs s = case break (== ',') s of
      (arg, []) -> [arg]
      (arg, _:rest) -> arg : splitArgs rest

astReturnTypeParser :: Parser String
astReturnTypeParser = do
  retType <- parseAnyCharExcept "\n"
  return (trimLine retType)

astConstructor :: Parser Ast
astConstructor = do
  _ <- parseString "func"
  _ <- parseMany (parseAnyChar " \t")
  name <- parseWhile (-1) "<"
  args <- astArgumentsParser
  _ <- parseWhile (-1) "=>"
  retType <- astReturnTypeParser
  let argsAst = [Var (astFindType t) n | arg <- args, let ws = words arg, length ws >= 2, let t = ws !! 0, let n = ws !! 1]
  return (Define name (Lambda argsAst (Symbol retType) (Symbol "body")))
