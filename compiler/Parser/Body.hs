{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Body
-}

module Parser.Body where

import Parsing
import DataTypes
import Parser.Tools
import Text.Read (readMaybe)
import Data.Char (isAlphaNum, ord, isDigit, isSpace)
import Control.Applicative ((<|>))
import Data.Maybe (isJust, fromMaybe)
import Data.List (find, isPrefixOf, uncons, unsnoc)

type ParseResult = Either String Ast

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

parseBodyHeader :: Parser String
parseBodyHeader = do
  _ <- parseString "$"
  _ <- parseMany (parseAnyChar " \t")
  funcName <- parseAnyCharExcept "\n"
  _ <- parseMany (parseAnyChar " \t\n")
  return funcName

findLastOpWorker :: String -> [String] -> Int -> Maybe (String, Int) -> String -> Maybe (String, Int)
findLastOpWorker _ _ _ bestOp [] = bestOp
findLastOpWorker originalStr ops level bestOp (c:cs) =
  let newLevel = case c of
                   '(' -> level + 1
                   ')' -> level - 1
                   _   -> level
      currentPos = length originalStr - length (c:cs)
      newBestOp  = if level == 0
                   then case find (`isPrefixOf` (c:cs)) ops of
                          Just foundOp -> Just (foundOp, currentPos)
                          Nothing      -> bestOp
                   else bestOp
  in findLastOpWorker originalStr ops newLevel newBestOp cs

findLastOp :: String -> [String] -> Maybe (String, String, String)
findLastOp str ops =
  case findLastOpWorker str ops 0 Nothing str of
    Nothing -> Nothing
    Just (op, pos) ->
      let (left, rightWithOp) = splitAt pos str
          right = drop (length op) rightWithOp
      in Just (op, left, right)

parseCharLiteral :: String -> Maybe ParseResult
parseCharLiteral s =
    case s of
        ['\'', char, '\''] -> Just $ Right $ Literal (Int8 (fromIntegral (ord char)))
        ('\'' : _)         -> Just $ Left $ "Syntax error: Invalid or unterminated character literal: " ++ s
        _                  -> Nothing

parseNumericOrBool :: String -> Maybe ParseResult
parseNumericOrBool s = (parseNum s) <|> (parseBool s)
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
    | otherwise                    = Right $ Symbol s

parseAtom :: [Ast] -> [Ast] -> String -> ParseResult
parseAtom env localArgs s =
    fromMaybe (parseSymbolOrCall env localArgs s) $
        parseCharLiteral s <|> parseNumericOrBool s

parseFunctionCall :: [Ast] -> [Ast] -> String -> ParseResult
parseFunctionCall env localArgs s =
  let (name, rest) = span isIdentifierChar s
      trimmedRest = trimLine rest
  in
    if null name then
      Left "Invalid function call: missing function name."
    else
        argStrings <- splitArgsBody trimmedRest
        argAsts <- traverse (parseExpression env localArgs) argStrings
        Right $ Call (Symbol name) argAsts

splitArgsBody :: String -> Either String [String]
splitArgsBody s = go (trimLine s) 0 "" []
  where
    go :: String -> Int -> String -> [String] -> Either String [String]
    go [] 0 "" acc = Right $ reverse acc
    go [] 0 current acc = Right $ reverse (trimLine current : acc)
    go [] level _ _ = Left $ "Syntax error: Unbalanced opening parentheses in function call. Level: " ++ show level

    go (c:cs) level current acc
        | level < 0 = Left "Syntax error: Unbalanced closing parentheses in function call."
        | c == '(' = go cs (level + 1) (current ++ [c]) acc
        | c == ')' = go cs (level - 1) (current ++ [c]) acc
        | c == ' ' && level == 0 =
            if null (trimLine current)
            then go cs 0 "" acc
            else go cs 0 "" (trimLine current : acc)
        | otherwise = go cs level (current ++ [c]) acc

parseFactor :: [Ast] -> [Ast] -> String -> ParseResult
parseFactor env localArgs s =
  let trimmed = trimLine s
  in case uncons trimmed of
       Just ('(', rest) ->
         case unsnoc rest of
           Just (middle, ')') -> parseOrOp env localArgs middle
           _ -> Left $ "Syntax error: Unterminated parenthesis in expression \"" ++ trimmed ++ "\""
       _ -> parseAtom env localArgs trimmed

parseUnary :: [Ast] -> [Ast] -> String -> ParseResult
parseUnary env localArgs s =
  let trimmed = trimLine s
  in
    if "-" `isPrefixOf` trimmed && not (isDigit (head (dropWhile isSpace (drop 1 trimmed)))) then
      let rest = drop 1 trimmed
      in do
        operand <- parseUnary env localArgs rest
        Right $ BinOp Neg [operand]
    else if "!" `isPrefixOf` trimmed then
      let rest = drop 1 trimmed
      in do
        operand <- parseUnary env localArgs rest
        Right $ BinOp Not [operand]
    else parseFactor env localArgs trimmed

parseTerm :: [Ast] -> [Ast] -> String -> ParseResult
parseTerm env localArgs s =
  case findLastOp s ["*", "/", "%"] of
    Just (op, left, right) -> do
      leftAst <- parseTerm env localArgs left
      rightAst <- parseUnary env localArgs right
      Right $ BinOp (astFindOperation op) [leftAst, rightAst]
    Nothing -> parseUnary env localArgs s

parseAddition :: [Ast] -> [Ast] -> String -> ParseResult
parseAddition env localArgs s =
  case findLastOp s ["+", "-"] of
    Just (op, left, right) ->
      if op == "-" && null (trimLine left)
      then parseUnary env localArgs s
      else do
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
    let trimmed = trimLine s
    in if null trimmed
       then Left "Cannot parse an empty expression."
       else parseOrOp env localArgs trimmed

splitIfElseBlocks :: [String] -> ([(String, [String])], [String])
splitIfElseBlocks [] = ([], [])
splitIfElseBlocks (l:ls) =
    case breakOnArrow (trimLine l) of
        Just (conditionPart, thenPartStr) ->
            let
                (blockLines, remainingLs) = span (not . isIfLine) ls
                thisThenBlock = if null thenPartStr
                                then map trimLine blockLines
                                else (trimLine thenPartStr) : map trimLine blockLines
            in
                if null conditionPart then
                    ([], thisThenBlock ++ remainingLs)
                else
                    let (nextIfs, finalElse) = splitIfElseBlocks remainingLs
                    in ((conditionPart, thisThenBlock) : nextIfs, finalElse)
        Nothing -> ([], l:ls)

breakOnArrow :: String -> Maybe (String, String)
breakOnArrow s = case breakOn "->" s of (pre, "->", post) -> Just (trimLine pre, trimLine post); _ -> Nothing

isIfLine :: String -> Bool
isIfLine = isJust . breakOnArrow . trimLine

collectUntilNextIf :: [String] -> ([String], [String])
collectUntilNextIf [] = ([], [])
collectUntilNextIf (l:ls)
    | isIfLine l = ([], l:ls)
    | otherwise  = let (collected, rest) = collectUntilNextIf ls in (trimLine l : collected, rest)

buildIfChain :: [Ast] -> [Ast] -> [(String, [String])] -> [String] -> ParseResult
buildIfChain _ _ [] [] = Right $ List []
buildIfChain env localArgs [] elseLines = do
    parseBlock env localArgs elseLines
buildIfChain env localArgs ((condStr, thenLines):restIfs) finalElseLines = do
    conditionAst <- if null condStr
                    then Right $ Literal (Bool True)
                    else parseExpression env localArgs condStr
    thenAst <- if null thenLines
                 then Left "Missing expression or statement after '->'."
                 else parseBlock env localArgs thenLines

    elseAst <- buildIfChain env localArgs restIfs finalElseLines
    Right $ If conditionAst thenAst elseAst

buildBodyAsts :: [Ast] -> [Ast] -> [String] -> ParseResult
buildBodyAsts env localArgs rawLines =
    let (ifBlocks, elseBlock) = splitIfElseBlocks rawLines
    in if null ifBlocks && null elseBlock
       then Left "Function body cannot be empty or contain only whitespace."
       else buildIfChain env localArgs ifBlocks elseBlock

isDefinedFunc :: [Ast] -> String -> Bool
isDefinedFunc env name = any isMatchingFunc env where
    isMatchingFunc (Define n (Lambda {})) = n == name
    isMatchingFunc _ = False

isLocalArg :: [Ast] -> String -> Bool
isLocalArg args name = any isMatchingArg args where
    isMatchingArg (Var _ varName) = varName == name
    isMatchingArg _ = False

isGlobalVar :: [Ast] -> String -> Bool
isGlobalVar env name = any isGlobalVarDef env where
    isGlobalVarDef (Define varName val) = varName == name && isNotFunction val
    isGlobalVarDef _ = False

isNotFunction :: Ast -> Bool
isNotFunction (Lambda _ _ _) = False
isNotFunction _ = True

getFuncArgs :: [Ast] -> String -> [Ast]
getFuncArgs env funcName = case find isMatchingFunc env of
    Just (Define _ (Lambda args _ _)) -> args
    _ -> []
  where isMatchingFunc (Define n (Lambda {})) = n == funcName; isMatchingFunc _ = False

fillBody :: [Ast] -> String -> [String] -> Either String [Ast]
fillBody env funcName rawBodyLines =
  if not (isDefinedFunc env funcName)
  then Left $ "Function \"" ++ funcName ++ "\" is not defined."
  else
    let localArgs = getFuncArgs env (removeSpaces funcName)
    in case buildBodyAsts env localArgs rawBodyLines of
      Left err -> Left $ "Error in body of function '" ++ funcName ++ "': " ++ err
      Right bodyAst -> Right $ map (patchFuncBody funcName bodyAst) env

patchFuncBody :: String -> Ast -> Ast -> Ast
patchFuncBody name bodyAst (Define n (Lambda args retType (Symbol "body")))
  | n == name = Define n (Lambda args retType bodyAst)
patchFuncBody _ _ other = other

parseLocalDefine :: [Ast] -> [Ast] -> String -> ParseResult
parseLocalDefine env localArgs s =
  case breakOn "=" s of
    (typeAndName, "=", value) ->
      case words (trimLine typeAndName) of
        [varType, varName] -> do
          valueAst <- parseExpression env localArgs (trimLine value)
          Right $ Define varName valueAst
        _ -> Left $ "Invalid local definition syntax in: \"" ++ s ++ "\""
    _ -> Left "Not a local definition."

parseStatement :: [Ast] -> [Ast] -> String -> ParseResult
parseStatement env localArgs s =
  let trimmed = trimLine s
  in
    if "->" `isPrefixOf` trimmed then
      Left "Unexpected '->' at the start of a statement inside a block."
    else
      case parseLocalDefine env localArgs s of
        Right ast -> Right ast
        Left _ -> parseExpression env localArgs s

parseBlock :: [Ast] -> [Ast] -> [String] -> ParseResult
parseBlock _ _ [] = Right (List [])
parseBlock env localArgs [line] = parseStatement env localArgs line
parseBlock env localArgs (line:restLines) = do
    stmt <- parseStatement env localArgs line
    let newLocalArgs = case stmt of
                         Define name _ -> (Var (String "") name) : localArgs
                         _             -> localArgs
    restAst <- parseBlock env newLocalArgs restLines
    case restAst of
      List restStmts -> Right $ List (stmt : restStmts)
      singleStmt     -> Right $ List [stmt, singleStmt]