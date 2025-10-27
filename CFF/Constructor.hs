{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Constructor
-}

module CFF.Constructor where

import Parsing
import CFF.Data
import CFF.Body
import CFF.Define
import CFF.ParseCFF (parse)
import Debug.Trace (trace)
import Control.Applicative ((<|>))

type Env = [Ast]

main :: IO ()
main = do
  mres <- parse
  let input = maybe [] fst mres
      finalEnv = parseAllLines input []
  mapM_ print finalEnv

parseAllLines :: [String] -> Env -> Env
parseAllLines [] env = env
parseAllLines (line:ls) env
  | "define" `elem` words line =
    case toEither (runParser astDefine line) of
      Right ast -> parseAllLines ls (env ++ [ast])
      Left err -> trace ("Error in define: " ++ err) $ parseAllLines ls env
  | "func" `elem` words line =
    case toEither (runParser astConstrutor line) of
      Right ast -> parseAllLines ls (env ++ [ast])
      Left err -> trace ("Error in func: " ++ err) $ parseAllLines ls env
  | "$" `elem` words line =
    case runParser parseBodyHeader line of
      Just (funcName, _) ->
        let (bodyLines, remainingLines) = collectBodyLines ls
            updatedEnv = fillBody env funcName bodyLines
        in parseAllLines remainingLines updatedEnv
      Nothing -> trace ("Failed to parse body header: " ++ line) $ parseAllLines ls env
  | otherwise = trace ("Unknown line format: " ++ line) $ parseAllLines ls env

collectBodyLines :: [String] -> ([String], [String])
collectBodyLines [] = ([], [])
collectBodyLines (l:ls)
  | isIndented l =
    let (body, rest) = collectBodyLines ls
    in (CFF.Body.trimLine l : body, rest)
  | otherwise = ([], l:ls)
  where
    isIndented s = case s of
      (c:_) -> c == ' ' || c == '\t'
      _ -> False

astArgumentsParser :: Parser [String]
astArgumentsParser = do
  args <- parseWhile (-1) ">"
  let argList = map trim (splitArgs args)
  return argList
  where
    splitArgs :: String -> [String]
    splitArgs [] = []
    splitArgs s = case break (== ',') s of
      (arg, []) -> [arg]
      (arg, _:rest) -> arg : splitArgs rest

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
  let argsAst = map (\arg -> let (t:n:_) = words arg in Var (astFindType t) n) args
  return (Define name (Lambda argsAst (Symbol retType) (Symbol "body")))
  where
    trim :: String -> String
    trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")