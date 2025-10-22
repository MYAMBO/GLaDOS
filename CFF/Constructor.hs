{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Construtor
-}

module CFF.Construtor where

import CFF.Data
import Parsing
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Control.Applicative ((<|>))
import CFF.Define

main :: IO ()
main = do
    --let input = "func fact<Int32 n, Int32 o> => Int32\n"
    let input = "define Double pi = 3.14159"
    let asts = parseAstLines input
    mapM_ print asts

cleanCommentLine :: String -> String
cleanCommentLine ('-':'-':rest) = dropWhile (== ' ') rest
cleanCommentLine s = s

parseAstLines :: String -> [Either String Ast]
parseAstLines input =
    map (parseLine . cleanCommentLine) (filter (not . null) (lines input))

toEither :: Maybe (Ast, String) -> Either String Ast
toEither (Just (ast, _)) = Right ast
toEither Nothing          = Left "Parse error"

parseLine :: String -> Either String Ast
parseLine line
    | "func" `elem` words line = toEither (runParser astConstrutor line)
    | "define" `elem` words line = toEither (runParser astDefine line)
    | otherwise = Left $ "Unknown line format: " ++ line

astArgumentsParser :: Parser [String]
astArgumentsParser = do
    args <- parseWhile (-1) ">"
    let argList = map trim (splitArgs args)
    return argList
  where
    splitArgs :: String -> [String]
    splitArgs [] = []
    splitArgs s  = case break (== ',') s of
        (arg, [])      -> [arg]
        (arg, _:rest)  -> arg : splitArgs rest
    trim :: String -> String
    trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

astReturnTypeParser :: Parser String
astReturnTypeParser = do
    retType <- parseAnyCharExcept "\n"
    return (trim retType)
  where
    trim :: String -> String
    trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

astConstrutor :: Parser Ast
astConstrutor = do
    _ <- parseString "func"
    _ <- parseMany (parseAnyChar " \t")
    name <- parseWhile (-1) "<"
    args <- astArgumentsParser
    _ <- parseWhile (-1) "=>"
    retType <- astReturnTypeParser
    let argsAst =  map (\arg -> let (t:n:_) = words arg in Var (astFindType t) (String n)) args
    return (Define name (Lambda argsAst (Symbol retType) (Lambda [] (Symbol "body") (Symbol "body"))))
  where
    trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")
