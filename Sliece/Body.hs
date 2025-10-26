{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Body
-}

module Sliece.Body where

import Parsing
import Sliece.Data
import Sliece.Tools
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, isJust)
import Data.List (stripPrefix, find, isPrefixOf)

parseBodyHeader :: Parser String
parseBodyHeader = do
  _ <- parseString "$"
  _ <- parseMany (parseAnyChar " \t")
  funcName <- parseAnyCharExcept "\n"
  _ <- parseMany (parseAnyChar " \t\n")
  return funcName

binaryOperators :: [String]
binaryOperators = ["==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "%"]

parseExpression :: String -> Ast
parseExpression exprString =
    let trimmedExpr = trimLine exprString
    in case findBinaryOp trimmedExpr of
        Just (op, left, right) ->
            BinOp (astFindOperation op) [parseAtom (trimLine left), parseAtom (trimLine right)]
        Nothing ->
            parseAtom trimmedExpr

findBinaryOp :: String -> Maybe (String, String, String)
findBinaryOp s =
    find (\op -> let (_, opFound, _) = breakOn op s in not (null opFound)) binaryOperators >>= \op ->
        let (left, opFound, right) = breakOn op s
        in Just (opFound, left, right)

parseAtom :: String -> Ast
parseAtom s =
    case readMaybe s of
        Just (n :: Int) -> Litteral (Int32 (fromIntegral n))
        Nothing ->
            case readMaybe s of
                Just (f :: Double) -> Litteral (Double f)
                Nothing ->
                    case readMaybe s of
                        Just (b :: Bool) -> Litteral (Bool b)
                        Nothing -> Symbol s

buildBodyAsts :: [String] -> Ast
buildBodyAsts [] = List []
buildBodyAsts rawLines =
    let (ifBlocks, elseBlock) = splitIfElseBlocks rawLines
    in buildIfChain ifBlocks elseBlock

splitIfElseBlocks :: [String] -> ([(String, [String])], [String])
splitIfElseBlocks [] = ([], [])
splitIfElseBlocks (l:ls) =
    case breakOnArrow (trimLine l) of
        Just (conditionPart, thenPartStr) ->
            let (thisThenLinesRaw, remainingLs) = collectUntilNextIf ls
                thisThenLines = if null thenPartStr then thisThenLinesRaw else thenPartStr : thisThenLinesRaw
                (nextIfBlocks, finalElseBlock) = splitIfElseBlocks remainingLs
            in ((conditionPart, thisThenLines) : nextIfBlocks, finalElseBlock)
        Nothing -> ([], l:ls)

breakOnArrow :: String -> Maybe (String, String)
breakOnArrow s =
    case breakOn "->" s of
        (pre, "->", post) -> Just (trimLine pre, trimLine post)
        _ -> Nothing

isIfLine :: String -> Bool
isIfLine s = isJust (breakOnArrow (trimLine s))

collectUntilNextIf :: [String] -> ([String], [String])
collectUntilNextIf [] = ([], [])
collectUntilNextIf (l:ls)
    | isIfLine l = ([], l:ls)
    | otherwise =
        let (collected, rest) = collectUntilNextIf ls
        in (trimLine l : collected, rest)

buildIfChain :: [(String, [String])] -> [String] -> Ast
buildIfChain [] [] = List []
buildIfChain [] elseLines =
    case map parseExpression elseLines of
        [singleExpr] -> singleExpr
        exprs -> List exprs
buildIfChain ((condStr, thenLines):restIfBlocks) finalElseLines =
    let conditionAst = if null condStr
                       then Litteral (Bool True)
                       else parseExpression condStr
        thenAst = case map parseExpression thenLines of
                    [singleExpr] -> singleExpr
                    exprs -> List exprs
        elseAst = buildIfChain restIfBlocks finalElseLines
    in If conditionAst thenAst elseAst

fillBody :: [Ast] -> String -> [String] -> [Ast]
fillBody env funcName rawBodyLines =
  let bodyAst = buildBodyAsts rawBodyLines 
  in map (patchFuncBody funcName bodyAst) env

patchFuncBody :: String -> Ast -> Ast -> Ast
patchFuncBody name bodyAst (Define n (Lambda args retType (Symbol "body")))
  | n == name = Define n (Lambda args retType bodyAst)
patchFuncBody _ _ other = other