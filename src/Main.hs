{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Core
-}

import Parsing (runParser)
import ParseSExpr (parseSExpr)
import System.Exit (exitSuccess)
import System.IO.Error (isEOFError, catchIOError)
import System.IO (hSetBuffering, BufferMode(..), stdout)

parenBalance :: String -> Int
parenBalance = foldl update 0
  where
    update n '(' = n + 1
    update n ')' = n - 1
    update n _   = n

readBalancedInput :: IO String
readBalancedInput = go 0 ""
  where
    go balance acc = do
      if balance == 0 && not (null acc)
        then return acc
        else if balance < 0
          then putStrLn "Error: Unmatched closing parenthesis" >> go 0 ""
          else do
            putStr (if null acc then "> " else "\t")
            catchIOError
                (do
                line <- getLine
                let newAcc = acc ++ " " ++ line
                    newBalance = parenBalance newAcc
                go newBalance newAcc
                )
                (\e -> if isEOFError e
                        then do
                        putStrLn ""
                        exitSuccess
                        else ioError e)

startParser :: IO ()
startParser = do
  input <- readBalancedInput
  case runParser parseSExpr input of
    Nothing  -> putStrLn "Parse error"
    Just exprs -> print exprs
  startParser

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    startParser