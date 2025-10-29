{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- VM Main
-}

module Main where

import DataTypes
import Compiler (compile)
import Parser.Constructor
import Debug.Trace (trace)
import Parser.Parse (parse)
import Parser.Tools (trimLine)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL

getAst :: Maybe ([String], String) -> [Ast]
getAst mres =
    let rawInput = maybe [] fst mres
        nonEmptyLines = filter (not . null . trimLine) rawInput
        finalEnv = parseAllLines nonEmptyLines []
    in if isMainHere finalEnv
       then finalEnv
       else trace "\n---\n[!] Error: 'main' function is missing.\n---" []

main :: IO ()
main = do
    files <- getArgs
    if null files then
        putStrLn "Please provide at least one input file."
    else do
        mres <- parse files
        let astList = getAst mres
        if null astList
        then putStrLn "Parsing failed or produced no output. Aborting compilation."
        else do
            putStrLn "--- Compiling Files with New Function Structure ---"
            mapM_ print astList
            putStrLn "--------------------------------------------------------"

            compilationResult <- compile astList

            case compilationResult of
                Left compileError -> do
                    putStrLn "\n❌ Compilation Failed:"
                    putStrLn compileError
                Right bytecode -> do
                    let outputFilename = "output.bin"
                    putStrLn $ "\n✅ Compilation Successful!"
                    BL.writeFile outputFilename bytecode
                    putStrLn $ "Bytecode written to '" ++ outputFilename ++ "'"
                    putStrLn "\nTo run the test, use the following command:"
                    putStrLn $ "  ./glados-vm " ++ outputFilename
