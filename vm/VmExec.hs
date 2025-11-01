{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- VmExec
-}

module VmExec where

import Control.Monad (when)
import Op
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get (runGetOrFail)
import Data.Int (Int64)

import BytecodeParser (getInstruction)

updateEnv :: String -> Val -> Env -> Either String Env
updateEnv name newVal (binding@(key, _):rest)
    | key == name = return $ (name, newVal) : rest
    | otherwise   = (binding:) <$> updateEnv name newVal rest
updateEnv name _ [] = Left $ "Error: variable '" ++ name ++ "' not defined in local scope."

exec :: Args -> Env -> B.ByteString -> Stack -> Either String Val
exec args globalEnv bytecode stack = exec' [] bytecode stack where
  exec' :: Env -> B.ByteString -> Stack -> Either String Val
  exec' localEnv codeBytes currentStack
    | B.null codeBytes = Left "Error: Program finished without a Return instruction"
    | otherwise =
        case runGetOrFail getInstruction codeBytes of
            Left (_, _, errMsg) -> Left $ "Bytecode execution error: " ++ errMsg
            Right (remainingBytes, _, instruction) ->
                handleInstruction localEnv instruction remainingBytes currentStack

  handleInstruction :: Env -> Instruction -> B.ByteString -> Stack -> Either String Val
  handleInstruction localEnv (Define name) nextBytes (val:stack) =
      let newLocalEnv = (name, val) : localEnv
      in exec' newLocalEnv nextBytes stack
  handleInstruction _ (Define _) _ [] = Left "Error: Define expects a value on the stack"

  handleInstruction localEnv (Assign name) nextBytes (val:stack) = do
      newLocalEnv <- updateEnv name val localEnv
      exec' newLocalEnv nextBytes stack
  handleInstruction _ (Assign _) _ [] = Left "Error: Assign expects a value on the stack"

  handleInstruction localEnv (PushFromEnv name) nextBytes stack =
      case lookup name localEnv of
          Just val -> exec' localEnv nextBytes (val:stack)
          Nothing -> case lookup name globalEnv of
              Just val -> exec' localEnv nextBytes (val:stack)
              Nothing -> Left $ "Error: variable '" ++ name ++ "' not found in any scope"

  handleInstruction localEnv (PushFromArgs index) nextBytes stack =
      if index >= 0 && index < length args
          then let argVal = args !! index
               in exec' localEnv nextBytes (argVal : stack)
          else Left $ "Runtime Error: Invalid argument index " ++ show index

  handleInstruction localEnv (Push val) nextBytes stack = exec' localEnv nextBytes (val:stack)
  handleInstruction localEnv (Pop) nextBytes (_:stack) = exec' localEnv nextBytes stack
  handleInstruction _ (Pop) _ [] = Left "Error: Pop: Stack is empty"

  handleInstruction _ (Return) _ (val:_) = return val
  handleInstruction _ (Return) _ [] = Left "Error: Ret: Stack is empty"

  handleInstruction localEnv (Jump n) remainingBytes stack =
      let offset = fromIntegral n
      in if offset > B.length remainingBytes
         then Left "Error: Jump offset is out of bounds"
         else exec' localEnv (B.drop offset remainingBytes) stack

  handleInstruction localEnv (JumpIfFalse n) remainingBytes (BoolVal False:stack) =
      let offset = fromIntegral n
      in if offset > B.length remainingBytes
         then Left "Error: JumpIfFalse offset is out of bounds"
         else exec' localEnv (B.drop offset remainingBytes) stack

  handleInstruction localEnv (JumpIfFalse _) nextBytes (BoolVal True:stack) =
      exec' localEnv nextBytes stack
  handleInstruction _ (JumpIfFalse _) _ (val:_) = Left $ "Error: JumpIfFalse expects a Boolean, got " ++ show val
  handleInstruction _ (JumpIfFalse _) _ [] = Left "Error: JumpIfFalse expects a value on the stack"

  handleInstruction localEnv (Call argCount) nextBytes stack = do
      when (length stack < argCount + 1) $
          Left "Error: Call stack underflow"
      let (argsPopped, funcAndRest) = splitAt argCount stack
      case funcAndRest of
          (func:deeperStack) -> do
              res <- execCall argCount func globalEnv argsPopped deeperStack
              exec' localEnv nextBytes (res:deeperStack)
          [] -> Left "Error: Corrupted stack, function not found"

  handleInstruction _ (TailCall argCount) remainingBytes stack = do
      when (length stack < argCount + 1) $
          Left "Error: TailCall stack underflow"
      let (argsPopped, funcAndRest) = splitAt argCount stack
      case funcAndRest of
          (func:_) -> execCall argCount func globalEnv (reverse argsPopped) []  -- Tail call: reuse current args and return value directly
          [] -> Left "Error: Corrupted stack, function not found for TailCall"

  handleInstruction _ instr _ _ = Left $ "Instruction not yet implemented in VM: " ++ show instr


execCall :: Int -> Val -> Env -> Args -> Stack -> Either String Val
execCall _ (Op op) _ args _ = do
    resultStack <- execOp op (reverse args)
    case resultStack of
        [res] -> return res
        _     -> Left "Error: Operation did not return a single value"
execCall argCount (Func program) globalEnv argsPopped _ = do
    when (length argsPopped /= argCount) $
        Left "Error: Incorrect number of arguments for function call"
    exec (reverse argsPopped) globalEnv program []
execCall _ val _ _ _ = Left $ "Error: Attempted to call a non-functional value: " ++ show val

isDouble :: Val -> Bool
isDouble (DblVal _) = True
isDouble _          = False

isFloat :: Val -> Bool
isFloat (FltVal _) = True
isFloat _          = False

valToDouble :: Val -> Maybe Double
valToDouble (Int8Val v) = Just (fromIntegral v); valToDouble (Int16Val v) = Just (fromIntegral v)
valToDouble (Int32Val v) = Just (fromIntegral v); valToDouble (Int64Val v) = Just (fromIntegral v)
valToDouble (Word8Val v) = Just (fromIntegral v); valToDouble (Word16Val v) = Just (fromIntegral v)
valToDouble (Word32Val v) = Just (fromIntegral v); valToDouble (Word64Val v) = Just (fromIntegral v)
valToDouble (FltVal v) = Just (realToFrac v); valToDouble (DblVal v) = Just v; valToDouble _ = Nothing

valToFloat :: Val -> Maybe Float
valToFloat (Int8Val v) = Just (fromIntegral v); valToFloat (Int16Val v) = Just (fromIntegral v)
valToFloat (Int32Val v) = Just (fromIntegral v); valToFloat (Int64Val v) = Just (fromIntegral v)
valToFloat (Word8Val v) = Just (fromIntegral v); valToFloat (Word16Val v) = Just (fromIntegral v)
valToFloat (Word32Val v) = Just (fromIntegral v); valToFloat (FltVal v) = Just v; valToFloat _ = Nothing

valToInt64 :: Val -> Maybe Int64
valToInt64 (Int8Val v) = Just (fromIntegral v); valToInt64 (Int16Val v) = Just (fromIntegral v)
valToInt64 (Int32Val v) = Just (fromIntegral v); valToInt64 (Int64Val v) = Just v
valToInt64 (Word8Val v) = Just (fromIntegral v); valToInt64 (Word16Val v) = Just (fromIntegral v)
valToInt64 (Word32Val v) = Just (fromIntegral v); valToInt64 _ = Nothing

applyBinaryOp :: (Int64 -> Int64 -> Val) -> (Float -> Float -> Val) -> (Double -> Double -> Val)
              -> Val -> Val -> Stack -> Either String Stack
applyBinaryOp intOp fltOp dblOp v1 v2 stack
    | isDouble v1 || isDouble v2 =
        case (valToDouble v1, valToDouble v2) of
            (Just d1, Just d2) -> Right $ dblOp d1 d2 : stack
            _ -> Left "Error: Type mismatch during Double promotion"
    | isFloat v1 || isFloat v2 =
        case (valToFloat v1, valToFloat v2) of
            (Just f1, Just f2) -> Right $ fltOp f1 f2 : stack
            _ -> Left "Error: Type mismatch during Float promotion"
    | otherwise =
        case (valToInt64 v1, valToInt64 v2) of
            (Just i1, Just i2) -> Right $ intOp i1 i2 : stack
            _ -> Left $ "Error: Type mismatch. Cannot operate on " ++ show v1 ++ " and " ++ show v2

execOp :: Op -> Stack -> Either String Stack
execOp Add (v2:v1:stack) = applyBinaryOp (\a b -> Int64Val (a + b)) (\a b -> FltVal (a + b)) (\a b -> DblVal (a + b)) v2 v1 stack
execOp Mul (v2:v1:stack) = applyBinaryOp (\a b -> Int64Val (a * b)) (\a b -> FltVal (a * b)) (\a b -> DblVal (a * b)) v2 v1 stack
execOp Eq (v2:v1:stack) = applyBinaryOp (\a b -> BoolVal (a == b)) (\a b -> BoolVal (a == b)) (\a b -> BoolVal (a == b)) v2 v1 stack

execOp Sub (v2:v1:stack) = applyBinaryOp (\a b -> Int64Val (a - b)) (\a b -> FltVal (a - b)) (\a b -> DblVal (a - b)) v2 v1 stack
execOp Lt (v2:v1:stack) = applyBinaryOp (\a b -> BoolVal (a < b)) (\a b -> BoolVal (a < b)) (\a b -> BoolVal (a < b)) v2 v1 stack
execOp Gt (v2:v1:stack) = applyBinaryOp (\a b -> BoolVal (a > b)) (\a b -> BoolVal (a > b)) (\a b -> BoolVal (a > b)) v2 v1 stack

execOp Div (v2:v1:stack) =
    case (valToDouble v2, valToDouble v1) of
        (Just d2, Just d1) | d1 == 0 -> Left "Error: Division by zero" | otherwise -> Right $ DblVal (d2 / d1) : stack
        _ -> case (valToFloat v2, valToFloat v1) of
            (Just f2, Just f1) | f1 == 0 -> Left "Error: Division by zero" | otherwise -> Right $ FltVal (f2 / f1) : stack
            _ -> case (valToInt64 v2, valToInt64 v1) of
                (Just i2, Just i1) | i1 == 0 -> Left "Error: Division by zero" | otherwise -> Right $ Int64Val (i2 `div` i1) : stack
                _ -> Left $ "Error: Type mismatch for division on " ++ show v2 ++ " and " ++ show v1

execOp Neg (v:stack) = case v of
    Int8Val n -> Right $ Int8Val (negate n) : stack; Int16Val n -> Right $ Int16Val (negate n) : stack
    Int32Val n -> Right $ Int32Val (negate n) : stack; Int64Val n -> Right $ Int64Val (negate n) : stack
    FltVal n -> Right $ FltVal (negate n) : stack
    DblVal n -> Right $ DblVal (negate n) : stack
    _ -> Left $ "Error: Negation not supported for value: " ++ show v

execOp Not (BoolVal a : stack) = return $ BoolVal (not a) : stack
execOp And (BoolVal b : BoolVal a : stack) = return $ BoolVal (a && b) : stack
execOp Or (BoolVal b : BoolVal a : stack) = return $ BoolVal (a || b) : stack
execOp Xor (BoolVal b : BoolVal a : stack) = return $ BoolVal (a /= b) : stack
execOp Cons (v : List vs : stack) = return $ List (v:vs) : stack
execOp Car (List (v:_) : stack) = return $ v : stack
execOp Car (List [] : _) = Left "Error: Car cannot be called on an empty list"
execOp Cdr (List (_:vs) : stack) = return $ List vs : stack
execOp Cdr (List [] : _) = Left "Error: Cdr cannot be called on an empty list"
execOp EmptyList stack = return $ List [] : stack

execOp op stack = Left $ "Error: Invalid arguments on stack for operation " ++ show op ++ ". Stack: " ++ show (take 3 stack)
