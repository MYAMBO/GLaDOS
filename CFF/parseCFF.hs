{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- parseCFF
-}

module CFF.ParseCFF where

import Data
import Parsing
import Control.Applicative (Alternative(..))

parseFunc :: String -> Parser String
parseFunc prefix = prefix <$ pure prefix

parseFuncDeclaration :: String -> Parser String
parseFuncDeclaration prefix = prefix <$ pure prefix

parseDefine :: String -> Parser String
parseDefine prefix = prefix <$ pure prefix

parseElt :: Parser String
parseElt = parseString "$"
       <|> parseString "//"
       <|> parseString "/*"
       <|> parseString "*/"
       <|> parseString "func"
       <|> parseString "define"

parseNext :: Parser String
parseNext = parseWhileOneOf ["func", "define", "$", "//", "/*", "*/"]

parseCFF :: Parser [String]
parseCFF = do
    elt <- parseElt
    str <- parseNext
    str <- case elt of
        "func" -> parseFuncDeclaration ("func" ++ str)
        "define" -> parseDefine ("define" ++ str)
        "$" -> parseFunc ("$" ++ str)
        _ -> "" <$ pure ""
    other <- parseCFF
    return (str : other)
    <|> do
    a <- parseAnyCharExcept ""
    return [a]

startParseCFF :: Parser [String]
startParseCFF = do
    parseNext
    parseCFF

parseFile :: FilePath -> IO (Maybe ([String], String))
parseFile path = do
    content <- readFile path
    return $ runParser startParseCFF content

start :: IO (Maybe ([String], String))
start = parseFile "exemple.cff"