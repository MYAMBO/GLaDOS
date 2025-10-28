{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Data
-}

module Parser.Data where

import Text.Read (readMaybe)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

toEither :: Maybe (Ast, String) -> Either String Ast
toEither (Just (ast, _)) = Right ast
toEither Nothing = Left "Parse error"

astFindType :: String -> VariableAst
astFindType "Int32"   = Int32 0
astFindType "Int64"   = Int64 0
astFindType "Int16"   = Int16 0
astFindType "Int8"    = Int8 0
astFindType "UInt32"  = UInt32 0
astFindType "UInt64"  = UInt64 0
astFindType "UInt16"  = UInt16 0
astFindType "UInt8"   = UInt8 0
astFindType "Float"   = Float 0.0
astFindType "Double"  = Double 0.0
astFindType "Bool"    = Bool False
astFindType "String"  = String ""
astFindType _         = String "error"

addValueToVar :: VariableAst -> String -> VariableAst
addValueToVar (Int8 _) val    = Int8 (read val :: Int8)
addValueToVar (Int16 _) val   = Int16 (read val :: Int16)
addValueToVar (Int32 _) val   = Int32 (read val :: Int32)
addValueToVar (Int64 _) val   = Int64 (read val :: Int64)
addValueToVar (UInt8 _) val   = UInt8 (read val :: Word8)
addValueToVar (UInt16 _) val  = UInt16 (read val :: Word16)
addValueToVar (UInt32 _) val  = UInt32 (read val :: Word32)
addValueToVar (UInt64 _) val  = UInt64 (read val :: Word64)
addValueToVar (Float _) val   = Float (read val :: Float)
addValueToVar (Double _) val  = Double (read val :: Double)
addValueToVar (Bool _) val    = Bool (read val :: Bool)
addValueToVar (String _) val  = String val

parseLiteral :: VariableAst -> String -> Maybe VariableAst
parseLiteral (Int32 _) s  = fmap Int32 (readMaybe s)
parseLiteral (Int64 _) s  = fmap Int64 (readMaybe s)
parseLiteral (Int16 _) s  = fmap Int16 (readMaybe s)
parseLiteral (Int8 _) s   = fmap Int8 (readMaybe s)
parseLiteral (UInt32 _) s = fmap UInt32 (readMaybe s)
parseLiteral (UInt64 _) s = fmap UInt64 (readMaybe s)
parseLiteral (UInt16 _) s = fmap UInt16 (readMaybe s)
parseLiteral (UInt8 _) s  = fmap UInt8 (readMaybe s)
parseLiteral (Float _) s  = fmap Float (readMaybe s)
parseLiteral (Double _) s = fmap Double (readMaybe s)
parseLiteral (Bool _) s   = fmap Bool (readMaybe s)
parseLiteral (String _) s = Just (String s)

astFindOperation :: String -> Builtins
astFindOperation "=="   = Equal
astFindOperation "!="   = NotEqual
astFindOperation "<"    = LessThan
astFindOperation ">"    = GreaterThan
astFindOperation "<="   = LessThanOrEqual
astFindOperation ">="   = GreaterThanOrEqual
astFindOperation "+"    = Add
astFindOperation "-"    = Subtract
astFindOperation "*"    = Multiply
astFindOperation "/"    = Divide
astFindOperation "%"    = Modulo
astFindOperation _      = UnknownOp

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

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
              | And
              | Neg
              | Or
              | Not
              | Xor
              | UnknownOp
  deriving (Show, Eq)

data Ast = Var VariableAst String -- variable type and name
         | Literal VariableAst
         | List [Ast]
         | BinOp Builtins [Ast]
         | Define String Ast
         | If Ast Ast Ast
         | Call Ast [Ast]
         | Symbol String
         | Lambda [Ast] Ast Ast -- arguments, return type, body
  deriving (Show, Eq)