{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Define
-}

module CFF.Define where

import CFF.Data
import Parsing
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import CFF.Tools (lotSpaceToOne)
import Control.Applicative ((<|>))

parseTypeAndName :: String -> (String, String)
parseTypeAndName s =
    let (varType, rest) = break (== ' ') s
        varName = trim rest
    in (varType, varName)
  where
    trim :: String -> String
    trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

astDefine :: Parser Ast
astDefine = do
    _ <- parseString "define"
    typeAndName <- parseWhile (-1) "="
    let (varType, varName) = parseTypeAndName (lotSpaceToOne (trim typeAndName))
    value <- parseAnyCharExcept "\n"
    let cleanValue = removeSpaces value
    return $ Define varName (Var (addValueToVar (astFindType varType) cleanValue) varName)
  where
    trim :: String -> String
    trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")
