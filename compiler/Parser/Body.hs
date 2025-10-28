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
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum, ord)
import Control.Applicative ((<|>))
import Data.List (find, isPrefixOf, uncons, unsnoc)

type ParseResult = Either String Ast

data ScanState = ScanState {
    level  :: Int,
    bestOp :: Maybe (String, Int)
}

parseBodyHeader :: Parser String
parseBodyHeader = do
  _ <- parseString "$"
  _ <- parseMany (parseAnyChar " \t")
  funcName <- parseAnyCharExcept "\n"
  _ <- parseMany (parseAnyChar " \t\n")
  return funcName

parseFunctionCall :: [Ast] -> [Ast] -> String -> ParseResult
parseFunctionCall env localArgs s =
  let (name, rest) = span isAlphaNum s
      trimmedRest = trimLine rest
  in
    if null name || null trimmedRest then
      Left $ "Invalid function call syntax for: \"" ++ s ++ "\""
    else
      case uncons trimmedRest of
        Just ('(', middleAndLast) ->
          case unsnoc middleAndLast of
            Just (middle, ')') -> do
              argsAst <- parseExpression env localArgs middle
              return $ Call (Symbol name) [argsAst]
            _ -> Left $ "Syntax error: Unterminated parenthesis in function call \"" ++ s ++ "\""
        _ -> do
          argsAsts <- traverse (parseExpression env localArgs) (words trimmedRest)
          return $ Call (Symbol name) argsAsts

parseCharLiteral :: String -> Maybe ParseResult
parseCharLiteral s =
    case s of
        ['\'', char, '\''] -> Just $ Right $ Literal (Int8 (fromIntegral (ord char)))
        ('\'' : _)         -> Just $ Left $ "Syntax error: Invalid or unterminated character literal: " ++ s
        _                  -> Nothing 

parseNumericOrBool :: String -> Maybe ParseResult
parseNumericOrBool s =
    (parseNum s) <|> (parseBool s)
    where
      parseNum str = case readMaybe str :: Maybe Double of
        Just f -> if f == fromInteger (round f)
                  then Just $ Right $ Literal (Int32 (round f))
                  else Just $ Right $ Literal (Double f)
        Nothing -> Nothing
      
      parseBool str = case readMaybe str :: Maybe Bool of
        Just b -> Just $ Right $ Literal (Bool b)
        Nothing -> Nothing

parseSymbolOrCall :: [Ast] -> [Ast] -> String -> ParseResult
parseSymbolOrCall env localArgs s
    | ' ' `elem` s || '(' `elem` s = parseFunctionCall env localArgs s
    | isDefinedFunc env s          = Right $ Call (Symbol s) []
    | isLocalArg localArgs s       = Right $ Symbol s
    | otherwise                    = Left $ "Error: Use of undeclared function or variable '" ++ s ++ "'"

parseAtom :: [Ast] -> [Ast] -> String -> ParseResult
parseAtom env localArgs s =
    fromMaybe (parseSymbolOrCall env localArgs s) $
        parseCharLiteral s <|> parseNumericOrBool s

scanChar :: String -> [String] -> ScanState -> (Char, Int) -> ScanState
scanChar str binaryOps state@(ScanState lvl _) (char, pos) =
  case char of
    '(' -> state { level = lvl + 1 }
    ')' -> state { level = lvl - 1 }
    _   -> if lvl == 0
           then
             case find (`isPrefixOf` drop pos str) binaryOps of
               Just op -> state { bestOp = Just (op, pos) }
               Nothing -> state
           else state

findBestOp :: String -> Maybe (String, Int)
findBestOp str =
    let scanner = scanChar str binaryOperators
    in bestOp $ foldl scanner (ScanState 0 Nothing) (zip str [0..])

findBinaryOp :: String -> Maybe (String, String, String)
findBinaryOp str =
  case findBestOp str of
    Nothing -> Nothing
    Just (op, pos) ->
      let (left, rightWithOp) = splitAt pos str
          right = drop (length op) rightWithOp
      in Just (op, left, right)

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
buildIfChain _ _ [] [] = Right $ List []
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

isIfLine :: String -> Bool
isIfLine s = isJust (breakOnArrow (trimLine s))

breakOnArrow :: String -> Maybe (String, String)
breakOnArrow s =
    case breakOn "->" s of
        (pre, "->", post) -> Just (trimLine pre, trimLine post)
        _ -> Nothing

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
