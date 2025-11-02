{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Body
-}


module Parser.Body where


import Data.List (span)
import Data.Maybe (isJust)

import Parsing
import DataTypes
import Parser.Tools
import Parser.Utils
import Parser.Statement (parseBlock)
import Parser.Expression (parseExpression)

type ParseResult = Either String Ast

parseBodyHeader :: Parser String
parseBodyHeader = do
  _ <- parseString "$"; _ <- parseMany (parseAnyChar " \t")
  fName <- parseAnyCharExcept "\n"; _ <- parseMany (parseAnyChar " \t\n")
  return fName

breakOnArrow :: String -> Maybe (String, String)
breakOnArrow s = case breakOn "->" s of (p, "->", a) -> Just (trimLine p, trimLine a); _ -> Nothing

isIfLine :: String -> Bool
isIfLine = isJust . breakOnArrow . trimLine

splitIfElseBlocks :: [String] -> ([(String, [String])], [String])
splitIfElseBlocks [] = ([], [])
splitIfElseBlocks (l:ls) =
    case breakOnArrow (trimLine l) of
        Just (cond, thenP) ->
            let (bLines, rLines) = span (not . isIfLine) ls
                thisBlock = if null thenP then map trimLine bLines else trimLine thenP : map trimLine bLines
            in if null cond then ([], thisBlock ++ rLines)
               else let (nextIfs, fElse) = splitIfElseBlocks rLines in ((cond, thisBlock) : nextIfs, fElse)
        Nothing -> ([], l:ls)

buildIfChain :: [Ast] -> [Ast] -> [(String, [String])] -> [String] -> ParseResult
buildIfChain _ _ [] [] = Right (List [])
buildIfChain env localArgs [] elseLines = parseBlock env localArgs elseLines
buildIfChain env localArgs ((cond, thenL):restIfs) finalElse = do
    condAst <- if null cond then Right (Literal (Bool True)) else parseExpression env localArgs cond
    thenAst <- if null thenL then Left "Missing expression after '->'." else parseBlock env localArgs thenL
    elseAst <- buildIfChain env localArgs restIfs finalElse
    Right $ If condAst thenAst elseAst

buildBodyAsts :: [Ast] -> [Ast] -> [String] -> ParseResult
buildBodyAsts env localArgs rawLines =
    let (ifBlocks, elseBlock) = splitIfElseBlocks rawLines
    in
      if not (null ifBlocks) && null elseBlock
      then Left "Syntax error: A conditional function body must end with a mandatory 'else' (->) clause."
      else if null ifBlocks && null elseBlock
      then Left "Function body cannot be empty or contain only whitespace."
      else buildIfChain env localArgs ifBlocks elseBlock

fillBody :: [Ast] -> String -> [String] -> Either String [Ast]
fillBody env funcName rawLines =
  if not (isDefinedFunc env funcName) then Left $ "Function \"" ++ funcName ++ "\" is not defined."
  else
    let localArgs = getFuncArgs env (removeSpaces funcName)
    in case buildBodyAsts env localArgs rawLines of
      Left err -> Left $ "Error in body of '" ++ funcName ++ "': " ++ err
      Right bodyAst -> Right $ map (patchFuncBody funcName bodyAst) env

patchFuncBody :: String -> Ast -> Ast -> Ast
patchFuncBody n b (Define defN (Lambda args rT (Symbol "body"))) | n == defN = Define defN (Lambda args rT b)
patchFuncBody _ _ other = other