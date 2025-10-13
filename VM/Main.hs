{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Vm Main
-}

module Main where

import System.Environment (getArgs)
import BytecodeParser (parseAndExec)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> parseAndExec filepath
        _          -> putStrLn "Usage: glados-vm <bytecode-file>"
