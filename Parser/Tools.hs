{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Tools
-}

module Tools where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


breakOn :: String -> String -> (String, String, String)
breakOn needle haystack = go haystack []
  where
    go [] acc = (reverse acc, "", "")
    go s acc
      | needle `isPrefixOf` s = (reverse acc, needle, drop (length needle) s)
      | otherwise = go (drop 1 s) (head s : acc)

lotSpaceToOne :: String -> String
lotSpaceToOne [] = []
lotSpaceToOne (' ':xs) = ' ' : lotSpaceToOne (dropWhile (== ' ') xs)
lotSpaceToOne (x:xs) = x : lotSpaceToOne xs

cleanCommentLine :: String -> String
cleanCommentLine ('-':'-':rest) = dropWhile (== ' ') rest
cleanCommentLine s = s

trimLine :: String -> String
trimLine s =
  reverse (dropWhile (`elem` " \t") (reverse (dropWhile (`elem` " \t") s)))
