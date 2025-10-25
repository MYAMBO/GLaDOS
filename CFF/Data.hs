{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Data
-}


module Data where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

data VariableAst = Int8 Int8
                 | Int16 Int16
                 | Int32 Int32
                 | Int64 Int64
                 | UInt8 Word8
                 | UInt16 Word16
                 | UInt32 Word32
                 | UInt64 Word64
                 | Float Float
                 | Double Double
                 | Bool Bool
                 | String String
  deriving (Show, Eq)

data Ast = Var VariableAst
             | List [Ast]
             | Define String Ast
             | If Ast Ast Ast
             | Call Ast [Ast]
             | Lambda [String] Ast
  deriving (Show, Eq)