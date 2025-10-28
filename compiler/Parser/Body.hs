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
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isJust)
import Data.List (stripPrefix, find, isPrefixOf)
import Data.Char (isSpace, isAlphaNum, ord)

type ParseResult = Either String Ast

parseBodyHeader :: Parser String
parseBodyHeader = do
  _ <- parseString "$"
  _ <- parseMany (parseAnyChar " \t")
  funcName <- parseAnyCharExcept "\n"
  _ <- parseMany (parseAnyChar " \t\n")
  return funcName

binaryOperators :: [String]
binaryOperators = ["==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "%"]

parseFunctionCall :: [Ast] -> [Ast] -> String -> ParseResult
parseFunctionCall env localArgs s =
  let (name, rest) = span isAlphaNum s
      trimmedRest = trimLine rest
  in if null name || null trimmedRest
     then Left $ "Invalid function call syntax for: \"" ++ s ++ "\""
     else if not (null trimmedRest) && head trimmedRest == '(' && last trimmedRest == ')' then
        let argsStr = take (length trimmedRest - 2) (tail trimmedRest)
        in do
             argsAst <- parseExpression env localArgs argsStr
             return $ Call (Symbol name) [argsAst]
     else do
        argsAsts <- traverse (parseExpression env localArgs) (words trimmedRest)
        return $ Call (Symbol name) argsAsts

parseAtom :: [Ast] -> [Ast] -> String -> ParseResult
parseAtom env localArgs s
    | not (null s) && head s == '\'' =
        if length s == 3 && last s == '\''
        then Right $ Literal (Int8 (fromIntegral (ord (s !! 1))))
        else Left $ "Syntax error: Invalid or unterminated character literal: " ++ s
    | otherwise =
        case readMaybe s :: Maybe Double of
            Just f ->
                if f == fromInteger (round f)
                then Right $ Literal (Int32 (round f))
                else Right $ Literal (Double f)
            Nothing ->
                case readMaybe s :: Maybe Bool of
                    Just b -> Right $ Literal (Bool b)
                    Nothing ->
                      if ' ' `elem` s || '(' `elem` s then
                        parseFunctionCall env localArgs s
                      else if isDefinedFunc env s then
                        Right $ Call (Symbol s) []
                      else if isLocalArg localArgs s then
                        Right $ Symbol s
                      else
                        Left $ "Error: Use of undeclared function or variable '" ++ s ++ "'"


data ScanState = ScanState {
    level  :: Int,
    bestOp :: Maybe (String, Int)
}

findBinaryOp :: String -> Maybe (String, String, String)
findBinaryOp str =
  case findBestOp str of
    Nothing -> Nothing
    Just (op, pos) ->
      let (left, rightWithOp) = splitAt pos str
          right = drop (length op) rightWithOp
      in Just (op, left, right)

findBestOp :: String -> Maybe (String, Int)
findBestOp str =
    bestOp $ foldl scanChar (ScanState 0 Nothing) (zip str [0..])
    where
      scanChar :: ScanState -> (Char, Int) -> ScanState
      scanChar state@(ScanState lvl currentBest) (char, pos) =
        case char of
          '(' -> state { level = lvl + 1 }
          ')' -> state { level = lvl - 1 }
          _   -> if lvl == 0
                 then
                   case find (`isPrefixOf` drop pos str) binaryOperators of
                     Just op -> state { bestOp = Just (op, pos) }
                     Nothing -> state
                 else state

parseExpression :: [Ast] -> [Ast] -> String -> ParseResult
parseExpression env localArgs exprString =
    let trimmedExpr = trimLine exprString
    in if null trimmedExpr
       then Left "Cannot parse an empty expression."
       else case findBinaryOp trimmedExpr of
            Just (op, left, right) -> do
                let leftTrimmed = trimLine left
                let rightTrimmed = trimLine right
                if op == "-" && null leftTrimmed then
                    parseAtom env localArgs trimmedExpr
                else if null leftTrimmed || null rightTrimmed then
                    Left $ "Incomplete expression for operator '" ++ op ++ "' in: \"" ++ trimmedExpr ++ "\""
                else do
                    leftAst <- parseExpression env localArgs leftTrimmed
                    rightAst <- parseExpression env localArgs rightTrimmed
                    Right $ BinOp (astFindOperation op) [leftAst, rightAst]
            Nothing -> parseAtom env localArgs trimmedExpr


buildBodyAsts :: [Ast] -> [Ast] -> [String] -> ParseResult
buildBodyAsts env localArgs rawLines =
    let (ifBlocks, elseBlock) = splitIfElseBlocks rawLines
    in if null ifBlocks && null elseBlock
       then Left "Function body cannot be empty or contain only whitespace."
       else buildIfChain env localArgs ifBlocks elseBlock

buildIfChain :: [Ast] -> [Ast] -> [(String, [String])] -> [String] -> ParseResult
buildIfChain env localArgs [] [] = Right $ List []
buildIfChain env localArgs [] elseLines = do
    exprs <- traverse (parseExpression env localArgs) elseLines
    Right $ case exprs of
        [singleExpr] -> singleExpr
        _            -> List exprs
buildIfChain env localArgs ((condStr, thenLines):restIfBlocks) finalElseLines = do
    conditionAst <- if null condStr
                    then Right $ Literal (Bool True)
                    else parseExpression env localArgs condStr
    thenExprs <- if null thenLines
                 then Left "Missing expression after '->' in if-then statement."
                 else traverse (parseExpression env localArgs) thenLines
    let thenAst = case thenExprs of
                    [singleExpr] -> singleExpr
                    _            -> List thenExprs
    elseAst <- buildIfChain env localArgs restIfBlocks finalElseLines
    Right $ If conditionAst thenAst elseAst

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

isDefinedFunc :: [Ast] -> String -> Bool
isDefinedFunc env name = any isMatchingFunc env
  where
    isMatchingFunc (Define n (Lambda {})) = n == name
    isMatchingFunc _                      = False

isLocalArg :: [Ast] -> String -> Bool
isLocalArg args name = any isMatchingArg args
  where
    isMatchingArg (Var _ varName) = varName == name
    isMatchingArg _               = False

getFuncArgs :: [Ast] -> String -> [Ast]
getFuncArgs env funcName =
  case find isMatchingFunc env of
    Just (Define _ (Lambda args _ _)) -> args
    _                                 -> []
  where
    isMatchingFunc (Define n (Lambda {})) = n == funcName
    isMatchingFunc _                      = False
    
isGoodName :: [Ast] -> String -> Bool
isGoodName env name =
  case find isMatchingFunc env of
    Just _  -> True
    Nothing -> False
  where
    isMatchingFunc (Define n (Lambda _ _ _)) = n == name
    isMatchingFunc _                          = False

fillBody :: [Ast] -> String -> [String] -> Either String [Ast]
fillBody env funcName rawBodyLines =
  if not (isDefinedFunc env funcName)
  then Left $ "Function \"" ++ funcName ++ "\" is not defined."
  else
    let localArgs = getFuncArgs env funcName
    in case buildBodyAsts env localArgs rawBodyLines of
      Left err -> Left $ "Error in body of function '" ++ funcName ++ "': " ++ err
      Right bodyAst ->
        let updatedEnv = map (patchFuncBody funcName bodyAst) env
        in Right updatedEnv

patchFuncBody :: String -> Ast -> Ast -> Ast
patchFuncBody name bodyAst (Define n (Lambda args retType (Symbol "body")))
  | n == name = Define n (Lambda args retType bodyAst)
patchFuncBody _ _ other = other
