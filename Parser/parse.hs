{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- parse
-}

module Parser.Parse where

import Parser.Data
import Parsing
import Control.Applicative (Alternative(..))

parseFuncLines :: Parser [String]
parseFuncLines = do
    _ <- parseString "\n    "
    line <- parseWhileOneOf ["\n"]
    let line' = dropWhile (`elem` " \t\n") line
    rest <- parseFuncLines
    return (("   " ++ line') : rest)
    <|> return []

parseFunc :: String -> Parser [String]
parseFunc prefix = do
    rest <- parseFuncLines
    return (prefix : rest)

parseFuncDeclaration :: String -> Parser [String]
parseFuncDeclaration prefix = [prefix] <$ pure prefix

parseDefine :: String -> Parser [String]
parseDefine prefix = [prefix] <$ pure prefix

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
    _ <- parseMany (parseAnyChar " \t\n")
    elt <- parseElt
    str <- case elt of
        "func" -> do
            a <- parseWhileOneOf ["\n"]
            parseFuncDeclaration ("func" ++ a)
        "define" -> do
            a <- parseWhileOneOf ["\n"]
            parseDefine ("define" ++ a)
        "$" -> do
            name <- parseWhileOneOf ["\n", "\t"]
            parseFunc ('$' : name)
        "//" -> do
            _ <- parseWhileOneOf ["\n"]
            return [""]
        "/*" -> do
            _ <- parseWhile (-1) "*/"
            return [""]
        _ -> [""] <$ pure ""
    other <- parseCFF
    return (str ++ other)
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

parse :: IO (Maybe ([String], String))
parse = parseFile "exemple.cff"