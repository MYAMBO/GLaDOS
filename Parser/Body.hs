{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Body
-}

module Parser.Body where

import Parsing
import Parser.Data
import Parser.Tools
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, isJust)
import Data.List (stripPrefix, find, isPrefixOf)
import Data.Char (isSpace, isAlphaNum, ord)

parseBodyHeader :: Parser String
parseBodyHeader = do
  _ <- parseString "$"
  _ <- parseMany (parseAnyChar " \t")
  funcName <- parseAnyCharExcept "\n"
  _ <- parseMany (parseAnyChar " \t\n")
  return funcName

binaryOperators :: [String]
binaryOperators = ["==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "%"]

parseFunctionCall :: String -> Maybe Ast
parseFunctionCall s =
  let (name, rest) = span isAlphaNum s
      trimmedRest = trimLine rest
  in if null name || null trimmedRest then Nothing
     else if head trimmedRest == '(' && last trimmedRest == ')' then
       let argsStr = take (length trimmedRest - 2) (tail trimmedRest)
           argsAst = [parseExpression argsStr]
       in Just $ Call (Symbol name) argsAst
     else
       -- Gère `fact 5`
       let argsAst = map parseExpression (words trimmedRest)
       in Just $ Call (Symbol name) argsAst

parseAtom :: String -> Ast
parseAtom s
    | length s == 3 && head s == '\'' && last s == '\'' =
        Literal (Int8 (fromIntegral (ord (s !! 1))))
    | otherwise = fromMaybe
        (fromMaybe (Symbol s) (parseFunctionCall s)) $
        (Literal . Int32 . fromIntegral <$> (readMaybe s :: Maybe Int))
        <|> (Literal . Double <$> (readMaybe s :: Maybe Double))
        <|> (Literal . Bool <$> (readMaybe s :: Maybe Bool))

findBinaryOp :: String -> Maybe (String, String, String)
findBinaryOp s = findOp s 0 Nothing
  where
    findOp [] _ result = result
    findOp (c:cs) level result
      | c == '(' = findOp cs (level + 1) result
      | c == ')' = findOp cs (level - 1) result
      | level == 0 =
          case find (`isPrefixOf` (c:cs)) binaryOperators of
            Just op ->
              let (left, _, right) = breakOn op s
              in findOp cs level (Just (op, left, right))
            Nothing -> findOp cs level result
      | otherwise = findOp cs level result

parseExpression :: String -> Ast
parseExpression exprString =
    let trimmedExpr = trimLine exprString
    in if head trimmedExpr == '(' && last trimmedExpr == ')'
        then parseExpression (take (length trimmedExpr - 2) (tail trimmedExpr))
        else case findBinaryOp trimmedExpr of
          Just (op, left, right) ->
            if null (trimLine left) && op == "-"
            then parseAtom (op ++ right)
            else BinOp (astFindOperation op) [parseExpression left, parseExpression right]
          Nothing -> parseAtom trimmedExpr

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
                       then Literal (Bool True)
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