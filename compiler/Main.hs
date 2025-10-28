{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- VM Main
-}

module Main where

import Compiler (compile)
import System.Environment (getArgs)
import Constructor (parseAllLines)
import Parse (parse)
import Data (Ast(..), Builtins(..), VariableAst(..))
import qualified Data.ByteString.Lazy as BL

getAst :: (Maybe ([String], String)) -> [Ast]
getAst lines = parseAllLines (maybe [] (filter (not . null) . fst) lines) []

main :: IO ()
main = do
    files <- getArgs
    mres <- parse files
    let astList = getAst mres

    putStrLn "--- Compiling Handmade AST with New Function Structure ---"
    mapM_ print astList
    putStrLn "--------------------------------------------------------"

    compilationResult <- compile astList

    case compilationResult of
        Left compileError -> do
            putStrLn "\n❌ Compilation Failed:"
            putStrLn compileError
        Right bytecode -> do
            let outputFilename = "output.gdbc"
            putStrLn $ "\n✅ Compilation Successful!"
            BL.writeFile outputFilename bytecode
            putStrLn $ "Bytecode written to '" ++ outputFilename ++ "'"
            putStrLn "\nTo run the test, use the following command:"
            putStrLn $ "  ./glados-vm " ++ outputFilename
