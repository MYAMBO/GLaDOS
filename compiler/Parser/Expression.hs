{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Expression
-}


module Parser.Expression where

import Data.Char (ord)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.List (find, isPrefixOf)

import DataTypes
import Parser.Tools
import Parser.Utils

type ParseResult = Either String Ast

findLastOpGo :: String -> [String] -> Int -> Maybe (String, Int) -> Int -> String -> Maybe (String, Int)
findLastOpGo _ _ _ bestOp _ "" = bestOp
findLastOpGo originalStr ops level bestOp currentPos s@(c:cs) =
  let newLevel = case c of
                   '(' -> level + 1
                   ')' -> level - 1
                   _   -> level
      isUnaryContext =
        currentPos == 0 || (let prev = last (trimLine (take currentPos originalStr)) in isOperatorChar prev || prev == '(')
      foundOp = if level == 0 then 
        find (`isPrefixOf` s) ops else Nothing
      newBestOp = case foundOp of
                    Just "-" | isUnaryContext -> bestOp
                    Just op -> Just (op, currentPos)
                    Nothing -> bestOp
  in findLastOpGo originalStr ops newLevel newBestOp (currentPos + 1) cs

findLastOp :: String -> [String] -> Maybe (String, String, String)
findLastOp str ops =
  case findLastOpGo str ops 0 Nothing 0 str of
    Nothing -> Nothing
    Just (op, pos) ->
      let (left, rightWithOp) = splitAt pos str
          right = drop (length op) rightWithOp
      in Just (op, left, right)

parseNum :: String -> Maybe ParseResult
parseNum str = case readMaybe str of
  Just f -> Just $ Right $ if f == fromInteger (round f) then Literal (Int32 (round f)) else Literal (Double f)
  Nothing -> Nothing

parseBool :: String -> Maybe ParseResult
parseBool str = case readMaybe str of Just b -> Just (Right (Literal (Bool b))); Nothing -> Nothing

parseCharLiteral :: String -> Maybe ParseResult
parseCharLiteral s = case s of
    ['\'', char, '\''] -> Just $ Right $ Literal (Int8 (fromIntegral (ord char)))
    ('\'' : _) -> Just $ Left $ "Syntax error: Invalid or unterminated character literal: " ++ s
    _ -> Nothing

parseNumericOrBool :: String -> Maybe ParseResult
parseNumericOrBool s = parseNum s <|> parseBool s

parseSymbol :: String -> ParseResult
parseSymbol s = Right $ Symbol s

parseAtom :: [Ast] -> [Ast] -> String -> ParseResult
parseAtom env localArgs s =
  case parseCharLiteral s <|> parseNumericOrBool s of
    Just result -> result
    Nothing ->
      if isDefinedFunc env s then
        Right $ Call (Symbol s) []
      else
        Right $ Symbol s

splitArgsGo :: Int -> String -> [String] -> String -> Either String [String]
splitArgsGo 0 "" acc "" = Right $ reverse acc
splitArgsGo 0 current acc "" = Right $ reverse (trimLine current : acc)
splitArgsGo level _ _ "" = Left $ "Syntax error: Unbalanced opening parentheses. Level: " ++ show level
splitArgsGo level current acc (c:cs)
    | level < 0 = Left "Syntax error: Unbalanced closing parentheses."
    | c == '(' = splitArgsGo (level + 1) (current ++ [c]) acc cs
    | c == ')' = splitArgsGo (level - 1) (current ++ [c]) acc cs
    | c == ' ' && level == 0 =
        if null (trimLine current) then 
            splitArgsGo 0 "" acc cs
        else 
            splitArgsGo 0 "" (trimLine current : acc) cs
    | otherwise = splitArgsGo level (current ++ [c]) acc cs

splitArgsBody :: String -> Either String [String]
splitArgsBody s = splitArgsGo 0 "" [] (trimLine s)

parseFunctionCall :: [Ast] -> [Ast] -> String -> ParseResult
parseFunctionCall env localArgs s =
  let (name, rest) = span isIdentifierChar s
  in if null name then Left "Invalid function call: missing function name."
     else if not (isDefinedFunc env name) then 
        Left $ "Error: Call to undefined function '" ++ name ++ "'"
     else do
        argStrings <- splitArgsBody (trimLine rest)
        argAsts <- traverse (parseExpression env localArgs) argStrings
        Right $ Call (Symbol name) argAsts

parseFactor :: [Ast] -> [Ast] -> String -> ParseResult
parseFactor env localArgs s =
  let trimmed = trimLine s in
  if "-" `isPrefixOf` trimmed then
    do operand <- parseFactor env localArgs (drop 1 trimmed); Right $ BinOp Neg [operand]
  else if "!" `isPrefixOf` trimmed then
    do operand <- parseFactor env localArgs (drop 1 trimmed); Right $ BinOp Not [operand]
  else if not (null trimmed) && head trimmed == '(' && last trimmed == ')' then
    parseExpression env localArgs (init (tail trimmed))
  else
    case parseNumericOrBool trimmed of
      Just (Right ast) -> Right ast
      Just (Left err) -> Left err
      Nothing ->
        let (potentialName, nameRest) = span isIdentifierChar trimmed
        in if not (null potentialName) && not (null (trimLine nameRest)) then
             parseFunctionCall env localArgs trimmed
           else
             parseAtom env localArgs trimmed

parseTerm :: [Ast] -> [Ast] -> String -> ParseResult
parseTerm env localArgs s =
  case findLastOp s ["*", "/", "%"] of
    Just (op, left, right) -> do
      leftAst <- parseTerm env localArgs left
      rightAst <- parseFactor env localArgs right
      Right $ BinOp (astFindOperation op) [leftAst, rightAst]
    Nothing -> parseFactor env localArgs s

parseAddition :: [Ast] -> [Ast] -> String -> ParseResult
parseAddition env localArgs s =
  case findLastOp s ["+", "-"] of
    Just (op, left, right) -> do
      leftAst <- parseAddition env localArgs left
      rightAst <- parseTerm env localArgs right
      Right $ BinOp (astFindOperation op) [leftAst, rightAst]
    Nothing -> parseTerm env localArgs s

parseComparison :: [Ast] -> [Ast] -> String -> ParseResult
parseComparison env localArgs s =
  case findLastOp s ["==", "!=", "<=", ">=", "<", ">"] of
    Just (op, left, right) -> do
      leftAst <- parseComparison env localArgs left
      rightAst <- parseAddition env localArgs right
      Right $ BinOp (astFindOperation op) [leftAst, rightAst]
    Nothing -> parseAddition env localArgs s

parseAndOp :: [Ast] -> [Ast] -> String -> ParseResult
parseAndOp env localArgs s =
  case findLastOp s ["&&"] of
    Just (op, left, right) -> do
      leftAst <- parseAndOp env localArgs left
      rightAst <- parseComparison env localArgs right
      Right $ BinOp (astFindOperation op) [leftAst, rightAst]
    Nothing -> parseComparison env localArgs s

parseOrOp :: [Ast] -> [Ast] -> String -> ParseResult
parseOrOp env localArgs s =
  case findLastOp s ["||"] of
    Just (op, left, right) -> do
      leftAst <- parseOrOp env localArgs left
      rightAst <- parseAndOp env localArgs right
      Right $ BinOp (astFindOperation op) [leftAst, rightAst]
    Nothing -> parseAndOp env localArgs s

parseExpression :: [Ast] -> [Ast] -> String -> ParseResult
parseExpression env localArgs s =
  let
    trimmedString = trimLine s
  in
    if null trimmedString
    then Left "Cannot parse an empty expression."
    else parseOrOp env localArgs trimmedString