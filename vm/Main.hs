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
import Op (Val(..))

-- Custom function to display only the value without the type constructor
showValue :: Val -> String
showValue (Int8Val v) = show v
showValue (Int16Val v) = show v
showValue (Int32Val v) = show v
showValue (Int64Val v) = show v
showValue (Word8Val v) = show v
showValue (Word16Val v) = show v
showValue (Word32Val v) = show v
showValue (Word64Val v) = show v
showValue (FltVal v) = show v
showValue (DblVal v) = show v
showValue (BoolVal v) = show v
showValue (Op _) = "<Op>"
showValue (Func _) = "<Func>"
showValue (List vs) = "[" ++ unwords (map showValue vs) ++ "]"

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
                Right finalVal -> putStrLn $ showValue finalVal
