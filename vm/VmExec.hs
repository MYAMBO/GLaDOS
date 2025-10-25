{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- VmExec
-}

module VmExec where

import Control.Monad (when)
import Op
import Data.Int (Int64)

-- | Helper function to update an existing variable in a local environment.
updateEnv :: String -> Val -> Env -> Either String Env
updateEnv name newVal (binding@(key, _):rest)
    | key == name = return $ (name, newVal) : rest
    | otherwise   = (binding:) <$> updateEnv name newVal rest
updateEnv name _ [] = Left $ "Error: variable '" ++ name ++ "' not defined in local scope."

-- | Main entry point for execution.
exec :: Args -> Env -> Program -> Stack -> Either String Val
exec args globalEnv = exec' [] where
  -- | The main execution loop, tracking local environment, args, and global env.
  exec' :: Env -> Program -> Stack -> Either String Val
  exec' localEnv (Define name:program) (val:stack) =
      let newLocalEnv = (name, val) : localEnv
      in exec' newLocalEnv program stack
  exec' _ (Define _:_) [] = Left "Error: Define expects a value on the stack"

  exec' localEnv (Assign name:program) (val:stack) = do
      newLocalEnv <- updateEnv name val localEnv
      exec' newLocalEnv program stack
  exec' _ (Assign _:_) [] = Left "Error: Assign expects a value on the stack"

  exec' localEnv (PushFromEnv name:program) stack =
      case lookup name localEnv of
          Just val -> exec' localEnv program (val:stack)
          Nothing -> case lookup name globalEnv of
              Just val -> exec' localEnv program (val:stack)
              Nothing -> Left $ "Error: variable '" ++ name ++ "' not found in any scope"

  exec' localEnv (Call argCount:program) stack = do
      when (length stack < argCount + 1) $
          Left $ "Error: Call expects " ++ show argCount ++ " arguments and a function on the stack"
      let (argsPopped, funcAndRest) = splitAt argCount stack
      case funcAndRest of
          (func:deeperStack) -> do
              newStack <- execCall argCount func globalEnv argsPopped deeperStack
              exec' localEnv program newStack
          [] -> Left "Error: Corrupted stack, function not found after arguments"

  exec' localEnv (TailCall argCount:program) stack = do
      when (length stack < argCount + 1) $
          Left $ "Error: TailCall expects " ++ show argCount ++ " arguments and a function on the stack"
      let (argsPopped, funcAndRest) = splitAt argCount stack
      case funcAndRest of
          (Func newProgram:_) ->
              exec (reverse argsPopped) globalEnv newProgram [] -- Tail call re-uses stack depth
          (Op op:deeperStack) ->
              execCall argCount (Op op) globalEnv argsPopped deeperStack >>= \newStack -> exec' localEnv program newStack
          (val:_) -> Left $ "Error: Attempted to tail call a non-functional value: " ++ show val
          [] -> Left "Error: Corrupted stack, function not found after arguments"

  exec' localEnv (PushFromArgs n:program) stack
    | n >= 0 && n < length args = exec' localEnv program (args !! n : stack)
    | otherwise = Left $ "Error: trying to access arg " ++ show n ++ " but only " ++ show (length args) ++ " were provided."

  exec' localEnv (Push val:program) stack = exec' localEnv program (val:stack)
  exec' localEnv (Pop:program) (_:stack) = exec' localEnv program stack
  exec' _ (Pop:_) [] = Left "Error: Pop: Stack is empty"
  exec' _ (Return:_) (val:_) = return val
  exec' _ (Return:_) [] = Left "Error: Return: Stack is empty"
  exec' localEnv (Jump n:program) stack = exec' localEnv (drop n program) stack
  exec' localEnv (JumpIfFalse n:program) (BoolVal False:stack) = exec' localEnv (drop n program) stack
  exec' localEnv (JumpIfFalse _:program) (BoolVal True:stack) = exec' localEnv program stack
  exec' _ (JumpIfFalse _:_) (val:_) = Left $ "Error: JumpIfFalse expects a Boolean value, but found " ++ show val
  exec' _ (JumpIfFalse _:_) [] = Left "Error: JumpIfFalse expects a value on the stack"
  exec' _ [] _ = Left "Error: Program finished without a Return instruction"

-- | Handles the logic for calling a value (user-defined function or built-in op).
execCall :: ArgCount -> Val -> Env -> Args -> Stack -> Either String Stack
execCall _ (Op op) _ args deeperStack = do
    resultStack <- execOp op args
    return (resultStack ++ deeperStack)
execCall argCount (Func program) globalEnv args deeperStack = do
    when (length args /= argCount) $
        Left $ "Error: Function expects " ++ show argCount ++ " arguments, but received " ++ show (length args)
    res <- exec (reverse args) globalEnv program []
    return (res : deeperStack)
execCall _ val _ _ _ = Left $ "Error: Attempted to call a non-functional value: " ++ show val

-- | Converts any numeric Val into a Double, if possible.
valToDouble :: Val -> Maybe Double
valToDouble (Int8Val   v) = Just (fromIntegral v)
valToDouble (Int16Val  v) = Just (fromIntegral v)
valToDouble (Int32Val  v) = Just (fromIntegral v)
valToDouble (Int64Val  v) = Just (fromIntegral v)
valToDouble (Word8Val  v) = Just (fromIntegral v)
valToDouble (Word16Val v) = Just (fromIntegral v)
valToDouble (Word32Val v) = Just (fromIntegral v)
valToDouble (Word64Val v) = Just (fromIntegral v)
valToDouble (FltVal    v) = Just (realToFrac v)
valToDouble (DblVal    v) = Just v
valToDouble _             = Nothing

-- | Converts any numeric Val into a Float, if possible.
valToFloat :: Val -> Maybe Float
valToFloat (Int8Val   v) = Just (fromIntegral v)
valToFloat (Int16Val  v) = Just (fromIntegral v)
valToFloat (Int32Val  v) = Just (fromIntegral v)
valToFloat (Int64Val  v) = Just (fromIntegral v)
valToFloat (Word8Val  v) = Just (fromIntegral v)
valToFloat (Word16Val v) = Just (fromIntegral v)
valToFloat (Word32Val v) = Just (fromIntegral v)
valToFloat (Word64Val v) = Just (fromIntegral v)
valToFloat (FltVal    v) = Just v
valToFloat _             = Nothing -- Cannot downcast Double to Float safely

-- | Converts any integral Val into an Int64, if possible.
valToInt64 :: Val -> Maybe Int64
valToInt64 (Int8Val   v) = Just (fromIntegral v)
valToInt64 (Int16Val  v) = Just (fromIntegral v)
valToInt64 (Int32Val  v) = Just (fromIntegral v)
valToInt64 (Int64Val  v) = Just v
valToInt64 (Word8Val  v) = Just (fromIntegral v)
valToInt64 (Word16Val v) = Just (fromIntegral v)
valToInt64 (Word32Val v) = Just (fromIntegral v)
valToInt64 _             = Nothing -- Cannot convert float/double to int

-- | Applies a binary operation with type promotion.
applyBinaryOp :: (Int64 -> Int64 -> Val) -> (Float -> Float -> Val) -> (Double -> Double -> Val)
              -> Val -> Val -> Stack -> Either String Stack
applyBinaryOp intOp fltOp dblOp v1 v2 stack =
    case (valToDouble v1, valToDouble v2) of
        (Just d1, Just d2) -> Right $ dblOp d2 d1 : stack
        _ -> case (valToFloat v1, valToFloat v2) of
            (Just f1, Just f2) -> Right $ fltOp f2 f1 : stack
            _ -> case (valToInt64 v1, valToInt64 v2) of
                (Just i1, Just i2) -> Right $ intOp i2 i1 : stack
                _ -> Left $ "Error: Type mismatch for operation. Cannot operate on " ++ show v1 ++ " and " ++ show v2

-- | Executes a built-in operation on the stack.
execOp :: Op -> Stack -> Either String Stack
-- Binary Numeric Operations
execOp Add (v1:v2:stack) = applyBinaryOp (\a b -> Int64Val (a + b)) (\a b -> FltVal (a + b)) (\a b -> DblVal (a + b)) v1 v2 stack
execOp Sub (v1:v2:stack) = applyBinaryOp (\a b -> Int64Val (a - b)) (\a b -> FltVal (a - b)) (\a b -> DblVal (a - b)) v1 v2 stack
execOp Mul (v1:v2:stack) = applyBinaryOp (\a b -> Int64Val (a * b)) (\a b -> FltVal (a * b)) (\a b -> DblVal (a * b)) v1 v2 stack
execOp Div (v1:v2:stack) =
    -- Division has special logic for zero checks and integer vs float behavior
    case (valToDouble v1, valToDouble v2) of
        (Just d1, Just d2) | d1 == 0 -> Left "Error: Division by zero"
                           | otherwise -> Right $ DblVal (d2 / d1) : stack
        _ -> case (valToFloat v1, valToFloat v2) of
            (Just f1, Just f2) | f1 == 0 -> Left "Error: Division by zero"
                               | otherwise -> Right $ FltVal (f2 / f1) : stack
            _ -> case (valToInt64 v1, valToInt64 v2) of
                (Just i1, Just i2) | i1 == 0 -> Left "Error: Division by zero"
                                   | otherwise -> Right $ Int64Val (i2 `div` i1) : stack
                _ -> Left $ "Error: Type mismatch for division. Cannot operate on " ++ show v1 ++ " and " ++ show v2

-- Comparison Operations
execOp Eq (v1:v2:stack) = applyBinaryOp (\a b -> BoolVal (a == b)) (\a b -> BoolVal (a == b)) (\a b -> BoolVal (a == b)) v1 v2 stack
execOp Lt (v1:v2:stack) = applyBinaryOp (\a b -> BoolVal (a < b)) (\a b -> BoolVal (a < b)) (\a b -> BoolVal (a < b)) v1 v2 stack
execOp Gt (v1:v2:stack) = applyBinaryOp (\a b -> BoolVal (a > b)) (\a b -> BoolVal (a > b)) (\a b -> BoolVal (a > b)) v1 v2 stack

-- Unary Negation
execOp Neg (v:stack) = case v of
    Int8Val  n -> Right $ Int8Val  (negate n) : stack
    Int16Val n -> Right $ Int16Val (negate n) : stack
    Int32Val n -> Right $ Int32Val (negate n) : stack
    Int64Val n -> Right $ Int64Val (negate n) : stack
    FltVal   n -> Right $ FltVal   (negate n) : stack
    DblVal   n -> Right $ DblVal   (negate n) : stack
    _          -> Left $ "Error: Negation is not supported for value: " ++ show v

-- Logical operations
execOp Not (BoolVal a : stack) = return $ BoolVal (not a) : stack
execOp And (BoolVal a : BoolVal b : stack) = return $ BoolVal (a && b) : stack
execOp Or  (BoolVal a : BoolVal b : stack) = return $ BoolVal (a || b) : stack
execOp Xor (BoolVal a : BoolVal b : stack) = return $ BoolVal (a /= b) : stack

-- List operations
execOp Cons (v : List vs : stack) = return $ List (v:vs) : stack
execOp Car (List (v:_) : stack) = return $ v : stack
execOp Car (List [] : _) = Left "Error: Car cannot be called on an empty list"
execOp Cdr (List (_:vs) : stack) = return $ List vs : stack
execOp Cdr (List [] : _) = Left "Error: Cdr cannot be called on an empty list"
execOp EmptyList stack = return $ List [] : stack

-- Fallback for insufficient arguments or type mismatches
execOp op []  = Left $ "Error: " ++ show op ++ " expects arguments, but the stack is empty."
execOp op [_] = Left $ "Error: Unexpected argument count for " ++ show op ++ ". This is a fallback for operations that do not match their expected patterns; most unary operations are handled above."
execOp op _   = Left $ "Error: Type mismatch for operation " ++ show op
