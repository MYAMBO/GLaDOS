{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Constructor
-}

module Parser.Constructor where

import Parsing
import DataTypes
import Parser.Body
import Parser.Tools
import Parser.Define
import Data.Char (isSpace)
import Debug.Trace (trace)

type Env = [Ast]

-- =============================================================================
-- CONSTRUCTOR PARSING IMPLEMENTATION
-- =============================================================================


isMainHere :: Env -> Bool
isMainHere [] = True
isMainHere env = any isMainFunc env
  where
    isMainFunc (Define "main" (Lambda _ _ _)) = True
    isMainFunc _                              = False

traceError :: String -> String -> Env -> Env
traceError errorType line env =
  let cleanedLine = dropWhile isSpace line
      firstLineOnly = takeWhile (/= '\n') cleanedLine
  in trace ("\n---\n[!] " ++ errorType ++ ".\n> In: \"" ++ firstLineOnly ++ "\"\n---") env

handleDefine :: String -> [String] -> Env -> Env
handleDefine line ls env =
  case toEither (runParser astDefine line) of
    Right ast -> parseAllLines ls (env ++ [ast])
    Left _    -> traceError "Syntax error in 'define' statement" line env

handleFunc :: String -> [String] -> Env -> Env
handleFunc line ls env =
  case toEither (runParser astConstructor line) of
    Right ast -> parseAllLines ls (env ++ [ast])
    Left _    -> traceError "Syntax error in 'func' statement" line env

handleBody :: String -> [String] -> Env -> Env
handleBody line ls env =
  case runParser parseBodyHeader line of
    Just (funcName, _) ->
      let (bodyLines, remainingLines) = collectBodyLines ls
      in
        case fillBody env funcName bodyLines of
          Left err -> trace ("\n---\n[!] " ++ err ++ "\n---") []
          Right updatedEnv -> parseAllLines remainingLines updatedEnv
    Nothing -> traceError "Error: Failed to parse function body header" line env

handleUnknown :: String -> Env
handleUnknown line =
  let cleanedLine = dropWhile isSpace line
      firstLineOnly = takeWhile (/= '\n') cleanedLine
  in trace ("\n---\n[!] Error: Unknown statement or syntax error.\n> In: \"" ++ firstLineOnly ++ "\"\n") $ []

parseAllLines :: [String] -> Env -> Env
parseAllLines [] env = env
parseAllLines (line:ls) env
  | null (trimLine line)       = parseAllLines ls env
  | "define" `elem` words line = handleDefine line ls env
  | "func" `elem` words line   = handleFunc line ls env
  | "$" `elem` words line      = handleBody line ls env
  | otherwise                  = handleUnknown line

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

astReturnTypeParser :: Parser String
astReturnTypeParser = do
  retType <- parseAnyCharExcept "\n"
  return (trimLine retType)

astConstructor :: Parser Ast
astConstructor = do
  _ <- parseString "func"
  _ <- parseMany (parseAnyChar " \t")
  name <- parseWhile (1) "<"
  args <- astArgumentsParser
  _ <- parseWhile (1) "=>"
  retType <- astReturnTypeParser
  let argsAst = [Var (astFindType t) n | arg <- args, let ws = words arg, length ws >= 2, let t = ws !! 0, let n = ws !! 1]
  return (Define name (Lambda argsAst (Symbol retType) (Symbol "body")))
