{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Data
-}

module CFF.Data where

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

data Builtins = Equal
              | NotEqual
              | LessThan
              | GreaterThan
              | LessThanOrEqual
              | GreaterThanOrEqual
              | Add
              | Subtract
              | Multiply
              | Divide
              | Modulo
  deriving (Show, Eq)

data Ast = Var VariableAst VariableAst -- variable type and name
         | List [Ast]
         | BinOp Builtins Ast Ast
         | Define String Ast
         | If Ast Ast Ast
         | Call Ast [Ast]
         | Symbol String
         | Lambda [Ast] Ast Ast -- arguments, return type, body
  deriving (Show, Eq)
