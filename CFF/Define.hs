{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Define
-}

module CFF.Define where

import CFF.Data
import Parsing
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Control.Applicative ((<|>))

--define double pi = 3.14159

lotSpaceToOne :: String -> String
lotSpaceToOne [] = []
lotSpaceToOne (' ':xs) = ' ' : lotSpaceToOne (dropWhile (== ' ') xs)
lotSpaceToOne (x:xs) = x : lotSpaceToOne xs

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
