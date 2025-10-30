{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Decompiler Main
-}

-- Main.hs
module Main (main) where

import System.Environment (getArgs)
import System.Exit        (exitFailure, exitSuccess)
import Decompiler         (writeDecompiled)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inPath, outPath] -> do
      result <- writeDecompiled inPath outPath
      case result of
        Left err -> do
          putStrLn $ "decompile error: " ++ err
          exitFailure
        Right () -> do
          putStrLn $ "wrote: " ++ outPath
          exitSuccess
    _ -> do
      putStrLn "Usage: decompiler <input.baka> <output.src>"
      exitFailure
