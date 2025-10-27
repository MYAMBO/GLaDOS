{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Define
-}

module Parser.Define where

import Parser.Data
import Parsing
import Parser.Tools (trim, lotSpaceToOne)
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

parseTypeAndName :: String -> (String, String)
parseTypeAndName s =
    let (varType, rest) = break (== ' ') s
        varName = trim rest
    in (varType, varName)

astDefine :: Parser Ast
astDefine = do
    _ <- parseString "define"
    typeAndName <- parseWhile (-1) "="
    let (varType, varName) = parseTypeAndName (lotSpaceToOne (trim typeAndName))
    value <- parseAnyCharExcept "\n"
    let cleanValue = removeSpaces value
    return $ Define varName (Var (addValueToVar (astFindType varType) cleanValue) varName)
