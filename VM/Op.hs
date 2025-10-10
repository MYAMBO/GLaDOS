module Op where

data Val = Num Int
    | Bool Bool
    | Op Op
    | Func Program
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
    deriving (Show, Eq)

type Stack = [Val]

type Program = [Instruction]

type Args = [Val]

type Env = [(String, Val)]

type ArgCount = Int
