{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- VM Main
-}

module Main where

import Compiler (compile)
import System.Environment (getArgs)
import Parser.Constructor
import Debug.Trace (trace)
import Parser.Parse (parse)
import Parser.Tools (trimLine)
import Parser.Data (Ast(..), Builtins(..), VariableAst(..))
import qualified Data.ByteString.Lazy as BL

-- Cette fonction prend le résultat brut du parsing de fichier et renvoie l'AST.
-- Elle gère le déballage du Maybe, le filtrage des lignes, et la vérification du main.
getAst :: Maybe ([String], String) -> [Ast]
getAst mres =
    let -- 1. On extrait les lignes brutes. Si c'est Nothing, on prend une liste vide.
        -- On suppose ici qu'on veut juste la première partie du tuple (les lignes).
        rawInput = maybe [] fst mres

        -- 2. On nettoie les lignes vides.
        nonEmptyLines = filter (not . null . trimLine) rawInput

        -- 3. On lance votre parseur principal.
        finalEnv = parseAllLines nonEmptyLines []

    -- 4. On vérifie si 'main' existe et on retourne le résultat approprié.
    in if isMainHere finalEnv
       then finalEnv
       else trace "\n---\n[!] Error: 'main' function is missing.\n---" []

main :: IO ()
main = do
    files <- getArgs
    if null files then -- 'null files' est plus idiomatique que 'files == []'
        putStrLn "Please provide at least one input file."
    else do
        mres <- parse files

        -- CORRECTION ICI : on utilise 'let' car getAst est une fonction pure.
        let astList = getAst mres

        -- Petite sécurité : si la liste est vide (à cause d'une erreur tracée plus tôt),
        -- on évite peut-être de lancer la compilation.
        if null astList
        then putStrLn "Parsing failed or produced no output. Aborting compilation."
        else do
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