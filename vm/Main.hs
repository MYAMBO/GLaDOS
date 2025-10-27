{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Vm Main
-}

module Main where

import System.Environment (getArgs)
import BytecodeParser (parseBytecode)
import VmExec (exec)
import Op (Val)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> runFile filepath
        _          -> putStrLn "Usage: glados-vm <bytecode-file>"

runFile :: FilePath -> IO ()
runFile path = do
    parseResult <- parseBytecode path
    case parseResult of
        Left parseErr -> putStrLn $ "Parse Error: " ++ parseErr
        Right (env, codeBody) ->
            case exec [] env codeBody [] of
                Left execErr -> putStrLn $ "Execution Error: " ++ execErr
                Right finalVal -> print finalVal
