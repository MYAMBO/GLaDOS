{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Parsing
-}

module Parsing where

import Control.Monad(guard)
import Data.List (isSuffixOf)
import Control.Applicative (Alternative(..))

type ParserType a = String -> Maybe (a, String)

data Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap = fmapParser

instance Applicative Parser where
    pure = pureParser
    (<*>) = appParser

instance Alternative Parser where
    empty = emptyParser
    (<|>) = altParser

instance Monad Parser where
    (>>=) = monadParser

fmapfunc :: (a -> b) -> ParserType a -> ParserType b
fmapfunc func p str | Just (a, str2) <- p str = Just (func a, str2)
fmapfunc _ _ _ = Nothing

fmapParser :: (a -> b) -> Parser a -> Parser b
fmapParser func (Parser p) = Parser (fmapfunc func p)

pureFunc :: a -> String -> Maybe (a, String)
pureFunc a str = Just (a, str)

pureParser :: a -> Parser a
pureParser a = Parser (pureFunc a)

appfunc :: ParserType (a -> b) -> ParserType a -> ParserType b
appfunc p1 p2 a | Just (f, b) <- p1 a, Just (d, c) <- p2 b = Just (f d, c)
appfunc _ _ _ = Nothing

appParser :: Parser (a -> b) -> Parser a -> Parser b
appParser (Parser p1) (Parser p2) = Parser (appfunc p1 p2)

emptyFunc :: String -> Maybe (a, String)
emptyFunc _ = Nothing

emptyParser :: Parser a
emptyParser = Parser emptyFunc

altfunc :: ParserType a -> ParserType a -> ParserType a
altfunc a _ str | Just c <- (a str) = Just c
altfunc _ b str = b str

altParser :: Parser a -> Parser a -> Parser a
altParser (Parser p1) (Parser p2) = Parser (altfunc p1 p2)

monadFunc :: ParserType a -> (a -> Parser b) -> ParserType b
monadFunc p func str | Just (a, str2) <- p str = runParser (func a) str2
monadFunc _ _ _ = Nothing

monadParser :: Parser a -> (a -> Parser b) -> Parser b
monadParser (Parser p) f = Parser (monadFunc p f)

tryMaybeNext :: ParserType a -> ParserType (Maybe a)
tryMaybeNext p str | Just (a, str2) <- p str = Just (Just a, str2)
tryMaybeNext _ str = Just (Nothing, str)

tryMaybe :: Parser a -> Parser (Maybe a)
tryMaybe (Parser p) = Parser (tryMaybeNext p)

parseCharNext :: Char -> ParserType Char
parseCharNext c (a:_) | c /= a = Nothing
parseCharNext _ (a:str) = Just (a, str)
parseCharNext _ _ = Nothing

parseChar :: Char -> Parser Char
parseChar a = Parser (parseCharNext a)

parseString :: String -> Parser String
parseString = traverse parseChar

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b = a <|> b

parserAnyCharFail :: ParserType a
parserAnyCharFail _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar (a:b) = parseOr (parseChar a) (parseAnyChar b)
parseAnyChar _ = Parser parserAnyCharFail

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func a b = func <$> a <*> b

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = parseAndWith (,)

parseMany :: Parser a -> Parser [a]
parseMany p = do
  a <- p
  b <- parseMany p
  return (a:b)
  <|> return []

parseSome :: Parser a -> Parser [a]
parseSome p = parseAndWith (:) p (parseMany p)

parseUInt :: Parser Int
parseUInt = do
    digits <- parseSome (parseAnyChar ['0'..'9'])
    return (read digits)

parseInt :: Parser Int
parseInt = do
    _ <- parseChar '-'
    digits <- parseUInt
    return (-digits)
    <|> do
    parseUInt

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = do
    _ <- parseChar '('
    a <- p
    _ <- parseChar ','
    b <- p
    _ <- parseChar ')'
    return (a, b)

parseTruple :: Parser (Int, Int, Int)
parseTruple = do
    _ <- parseChar '('
    a <- parseInt
    _ <- parseChar ','
    b <- parseInt
    _ <- parseChar ','
    c <- parseInt
    _ <- parseChar ')'
    return (a, b, c)

parseOneCharNext :: ParserType Char
parseOneCharNext (a:b) = Just (a, b)
parseOneCharNext _ = Nothing

parseOneChar :: Parser Char
parseOneChar = Parser parseOneCharNext

parseWhileToLast :: Int -> String -> Parser String
parseWhileToLast n s2 = do
    str1 <- parseWhile n s2
    str2 <- parseWhileToLast (n + 1) s2
    return (str1 ++ s2 ++ str2)
    <|> return ""

parseWhileNext :: Int -> String -> Parser String
parseWhileNext 0 _ = return ""
parseWhileNext n s2 = do
    str1 <- parseString s2
    str2 <- parseWhileNext (n - 1) s2
    return (str1 ++ str2)
    <|> do
    c <- parseOneChar
    str <- parseWhileNext n s2
    return (c : str)

parseWhile :: Int -> String -> Parser String
parseWhile (-1) s2 = do
    str <- parseWhileToLast 1 s2
    guard (s2 `isSuffixOf` str)
    return (take (length str - length s2) str)
parseWhile n s2 = do
    str <- parseWhileNext n s2
    return (take (length str - length s2) str)

parseBetween :: Int -> String -> String -> Parser String
parseBetween n s1 s2 = do
    _ <- parseString s1
    str <- parseWhile n s2
    return str

parseAnyCharExcept :: String -> Parser String
parseAnyCharExcept str = do
      c <- parseOneChar
      guard (c `notElem` str)
      str2 <- parseAnyCharExcept str
      return (c : str2)
      <|> return ""

parseWithoutConsumNext :: ParserType a -> ParserType String
parseWithoutConsumNext p input | Just _ <- p input = Just ("", input)
parseWithoutConsumNext _ _ = Nothing

parseWithoutConsum :: Parser a -> Parser String
parseWithoutConsum (Parser p) = Parser (parseWithoutConsumNext p)
