{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Define
-}

module Parser.Define where

import Parser.Data
import Parsing
import Debug.Trace (trace)
import Parser.Tools (lotSpaceToOne, trim)

-- =============================================================================
-- DEFINE PARSING IMPLEMENTATION
-- =============================================================================

parseTypeAndName :: String -> (String, String)
parseTypeAndName s =
    let (varType, rest) = break (== ' ') s
        varName = trim rest
    in (varType, varName)

isGoodType :: VariableAst -> Bool
isGoodType (String "error") = False
isGoodType _                = True

astDefine :: Parser Ast
astDefine = do
    _ <- parseString "define"
    typeAndName <- parseWhile (-1) "="
    let (varType, varName) = parseTypeAndName (lotSpaceToOne (trim typeAndName))
    if not (isGoodType (astFindType varType))
      then return $ trace ("Unknown type in define: " ++ varType) $ Define varName (Var (String "error") varName)
      else do
        value <- parseAnyCharExcept "\n"
        let cleanValue = removeSpaces value
        return $ Define varName (Var (addValueToVar (astFindType varType) cleanValue) varName)

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