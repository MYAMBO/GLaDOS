{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- ParseSExpr
-}

module ParseSExpr where

import Parsing
import Ast.Ast (SExpr(..))
import Control.Applicative (Alternative(..))

parseQuoted :: Parser String
parseQuoted = do
    _ <- parseChar '"'
    s <- parseAnyCharExcept "\""
    _ <- parseChar '"'
    return s

parseUnquoted :: Parser String
parseUnquoted = parseAnyCharExcept " )"

parseAtom :: Parser SExpr
parseAtom =
        (SAtom <$> parseInt)
    <|> (SSymbol <$> parseQuoted)
    <|> (SSymbol <$> parseUnquoted)

parseElement :: Parser SExpr
parseElement = (SList <$> parseSExprNext) <|> parseAtom

parseSExprList :: Parser [SExpr]
parseSExprList = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ')'
    return []
    <|> do
    _ <- parseMany (parseAnyChar " \n\t")
    elt <- parseElement
    rest <- parseSExprList
    return (elt : rest)

parseSExprNext :: Parser [SExpr]
parseSExprNext = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '('
    parseSExprList

parseSExpr :: Parser [SExpr]
parseSExpr = parseSExprNext
    <|> do
    _ <- parseMany (parseAnyChar " \n\t")
    res <- parseAtom
    return [res]