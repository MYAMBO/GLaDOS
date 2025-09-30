{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Core
-}

import Parsing (runParser)
import ParseSExpr (parseSExpr)
import Ast.Ast (sexprToAST, SExpr(..))
import Interpret (eval)
import DataStored (Env, Ast)
import System.Exit (exitSuccess)
import System.IO.Error (isEOFError, catchIOError)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import Ast.AstPrint (printAst)

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

printResult :: Maybe Ast -> IO ()
printResult (Just result) = putStrLn (printAst result)
printResult Nothing       = return ()

repl :: Env -> IO ()
repl env = do
    input <- readBalancedInput
    case runParser parseSExpr input of
        Nothing -> do
            putStrLn "Parse error"
            repl env
        Just ((SSymbol ""), _) -> repl env
        Just (sexpr, _) ->
            case sexprToAST sexpr of
                Nothing -> do
                    putStrLn "Error: Invalid expression structure"
                    repl env
                Just ast ->
                    case eval env ast of
                        Left err -> do
                            putStrLn err
                            repl env
                        Right (result, newEnv) -> do
                            printResult result
                            repl newEnv

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to the GLaDOS Lisp Interpreter!"
    repl []
