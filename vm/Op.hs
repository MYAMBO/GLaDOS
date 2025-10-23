{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Op
-}

module Op where

data Val = Num Int
    | Bool Bool
    | Op Op
    | Func Program
    | List [Val]
    deriving (Show, Eq)

data Op = Add
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

data Instruction = Push Val
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
