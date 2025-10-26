{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Op
-}

module Op where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString.Lazy as B

data Val
    = Int8Val   Int8
    | Int16Val  Int16
    | Int32Val  Int32
    | Int64Val  Int64
    | Word8Val  Word8
    | Word16Val Word16
    | Word32Val Word32
    | Word64Val Word64
    | FltVal    Float
    | DblVal    Double
    | BoolVal   Bool
    | Op        Op
    | Func      B.ByteString
    | List      [Val]
    deriving (Show, Eq)

data Op
    = Add | Sub | Mul | Div | Mod | Neg | Eq | Lt | Gt
    | Not | And | Or | Xor | Cons | Car | Cdr | EmptyList
    deriving (Show, Eq)

data Instruction
    = Push Val
    | Pop
    | Return
    | Jump Int
    | JumpIfFalse Int
    | Call ArgCount
    | TailCall ArgCount
    | PushFromArgs Int
    | PushFromEnv String
    | Define String
    | Assign String
    deriving (Show, Eq)

type Stack = [Val]
type Program = [Instruction]
type Args = [Val]
type Env = [(String, Val)]
type ArgCount = Int
