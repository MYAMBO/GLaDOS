{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- ParseSExpr
-}

module ParseSExpr where

import Parsing
import Control.Applicative (Alternative(..))

data SExpr =
      Atom Int
    | Symbol String
    | List [SExpr]
    deriving Show

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
        (Atom <$> parseInt)
    <|> (Symbol <$> parseQuoted)
    <|> (Symbol <$> parseUnquoted)

parseElement :: Parser SExpr
parseElement = (List <$> parseSExpr) <|> parseAtom

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

parseSExpr :: Parser [SExpr]
parseSExpr = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '('
    parseSExprList
    <|> do
    _ <- parseMany (parseAnyChar " \n\t")
    res <- parseAtom
    return [res]