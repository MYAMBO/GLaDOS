{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Tools
-}

module Parser.Tools where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- =============================================================================
-- TOOLS IMPLEMENTATION FOR PARSING
-- =============================================================================


breakOnGo :: String -> String -> String -> (String, String, String)
breakOnGo _ [] acc = (reverse acc, "", "")
breakOnGo needle s@(c:cs) acc
  | needle `isPrefixOf` s = (reverse acc, needle, drop (length needle) s)
  | otherwise = breakOnGo needle cs (c : acc)

breakOn :: String -> String -> (String, String, String)
breakOn needle haystack = breakOnGo needle haystack []

lotSpaceToOne :: String -> String
lotSpaceToOne [] = []
lotSpaceToOne (' ':xs) = ' ' : lotSpaceToOne (dropWhile (== ' ') xs)
lotSpaceToOne (x:xs) = x : lotSpaceToOne xs

cleanCommentLine :: String -> String
cleanCommentLine ('-':'-':rest) = dropWhile (== ' ') rest
cleanCommentLine s = s

trimLine :: String -> String
trimLine = reverse . dropWhile isSpace . reverse . dropWhile isSpace

trim :: String -> String
trim = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t")

splitArgs :: String -> [String]
splitArgs [] = []
splitArgs s = case break (== ',') s of
  (arg, []) -> [arg]
  (arg, _:rest) -> arg : splitArgs rest

binaryOperators :: [String]
binaryOperators = ["==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "%"]
