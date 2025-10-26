{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Main
-}

module Main where

import Compiler (compile)
import Ast (Ast(..), Builtins(..), VariableAst(..))
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    let handmadeAstList =
            [
              Define "add"
                (Lambda
                    [ Var (Int32 0) "a"
                    , Var (Int32 0) "b"
                    ]
                    (Symbol "Int32")
                    (BinOp Add [Symbol "a", Symbol "b"])
                )
            , Call (Symbol "add") [Literal (Int32 40), Literal (Int32 2)]
            ]

    putStrLn "--- Compiling Handmade AST with New Function Structure ---"
    mapM_ print handmadeAstList
    putStrLn "--------------------------------------------------------"

    compilationResult <- compile handmadeAstList

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
