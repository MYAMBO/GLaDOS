{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Define
-}

module Define where

import Data
import Parsing
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import Tools (lotSpaceToOne, trim)
import Control.Applicative ((<|>))

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
