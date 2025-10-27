{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Op
-}

module Op where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- | Represents all possible values in the language.
-- This has been expanded to support a rich set of numeric and boolean types.
data Val
    -- Numeric Types
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
    -- Boolean Type
    | BoolVal   Bool
    -- Operational and Structural Types
    | Op        Op
    | Func      Program
    | List      [Val]
    deriving (Show, Eq)

-- | Represents the built-in operations that can be performed.
data Op
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Neg
    | Eq
    | Lt
    | Gt
    | Not
    | And
    | Or
    | Xor
    | Cons
    | Car
    | Cdr
    | EmptyList
    deriving (Show, Eq)

-- | Represents a single instruction for the virtual machine.
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

-- | The execution stack, holding values.
type Stack = [Val]

-- | A sequence of instructions that makes up a program or function.
type Program = [Instruction]

-- | A list of values passed as arguments to a function.
type Args = [Val]

-- | The environment, mapping variable names to values.
type Env = [(String, Val)]

-- | The number of arguments for a function call.
type ArgCount = Int
