{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- VmExec
-}

module VmExec where

import Control.Monad (when)
import Op

-- Helper function to update an existing variable in an environment.
-- Fails if the variable is not found.
updateEnv :: String -> Val -> Env -> Either String Env
updateEnv name newVal [] = Left $ "Error: variable '" ++ name ++ "' not defined in local scope."
updateEnv name newVal ((key, _):rest)
    | key == name = return $ (key, newVal) : rest
updateEnv name newVal (binding:rest) = do
    newRest <- updateEnv name newVal rest
    return $ binding : newRest

-- Main entry point for execution. It takes the global environment
-- and kicks off the execution loop with an empty local environment.
exec :: Args -> Env -> Program -> Stack -> Either String Val
exec args globalEnv = exec' [] args globalEnv where
  -- The main execution loop. It now tracks localEnv, args, and globalEnv.
  exec' :: Env -> Args -> Env -> Program -> Stack -> Either String Val
  exec' localEnv args globalEnv (Define name:program) (val:stack) =
      let newLocalEnv = (name, val) : localEnv
      in exec' newLocalEnv args globalEnv program stack
  exec' _ _ _ (Define _:_) [] = Left "Error: Define expects a value on the stack"

  exec' localEnv args globalEnv (Assign name:program) (val:stack) = do
      newLocalEnv <- updateEnv name val localEnv
      exec' newLocalEnv args globalEnv program stack
  exec' _ _ _ (Assign _:_) [] = Left "Error: Assign expects a value on the stack"

  exec' localEnv args globalEnv (PushFromEnv s:program) stack =
      case lookup s localEnv of
          Just v -> exec' localEnv args globalEnv program (v:stack)
          Nothing -> case lookup s globalEnv of
              Just v -> exec' localEnv args globalEnv program (v:stack)
              Nothing -> Left $ "Error: variable '" ++ s ++ "' not found in any scope"

  exec' localEnv args globalEnv (Call argcount:program) stack = do
      when (length stack < argcount + 1) $
          Left $ "Error: Call expects " ++ show argcount ++ " arguments and a function on the stack"
      let (argsPopped, funcAndRest) = splitAt argcount stack
      case funcAndRest of
          (func:deeperStack) -> do
              newStack <- execCall argcount func globalEnv argsPopped deeperStack
              exec' localEnv args globalEnv program newStack
          [] -> Left "Error: Corrupted stack, function not found after arguments"

  exec' localEnv args globalEnv (TailCall argcount:program) stack = do
      when (length stack < argcount + 1) $
          Left $ "Error: Call expects " ++ show argcount ++ " arguments and a function on the stack"
      let (argsPopped, funcAndRest) = splitAt argcount stack
      case funcAndRest of
          (Func newProgram:deeperStack) -> do
              when (length argsPopped /= argcount) $
                  Left $ "Error: The function expects " ++ show argcount ++ " arguments, but received " ++ show (length argsPopped)
              let newArgs = reverse argsPopped
              -- The new function starts with its own args and a fresh local env.
              exec newArgs globalEnv newProgram deeperStack
          (Op op:deeperStack) ->
              -- Built-in Ops don't have a local scope, so they just return a value.
              execCall argcount (Op op) globalEnv argsPopped deeperStack >>= \newStack -> exec' localEnv args globalEnv program newStack
          (val:_) -> Left $ "Error: Attempted to call a non-functional value: " ++ show val
          [] -> Left "Error: Corrupted stack, function not found after arguments"

  exec' localEnv args globalEnv (PushFromArgs n:program) stack
    | n < length args = exec' localEnv args globalEnv program ((args !! n):stack)
    | otherwise       = Left $ "Error: trying to access arg " ++ show n ++ " but only " ++ show (length args) ++ " were provided."

  exec' localEnv args globalEnv (Push a:program) stack = exec' localEnv args globalEnv program (a:stack)
  exec' localEnv args globalEnv (Pop:program) (_:stack) = exec' localEnv args globalEnv program stack
  exec' _ _ _ (Pop:_) [] = Left "Error: Pop: Stack is empty"
  exec' _ _ _ (Return:_) (a:_) = return a
  exec' _ _ _ (Return:_) [] = Left "Error: Ret: Stack is empty"
  exec' localEnv args globalEnv (Jump n:program) stack = exec' localEnv args globalEnv (drop n program) stack
  exec' localEnv args globalEnv (JumpIfFalse n:program) (Bool False:stack) = exec' localEnv args globalEnv (drop n program) stack
  exec' localEnv args globalEnv (JumpIfFalse _:program) (Bool True:stack) = exec' localEnv args globalEnv program stack
  exec' _ _ _ (JumpIfFalse _:_) (val:_) = Left $ "Error: JumpIfFalse expects a Boolean value, but found " ++ show val
  exec' _ _ _ (JumpIfFalse _:_) [] = Left "Error: JumpIfFalse expects a value on the stack"
  exec' _ _ _ [] _ = Left "Error: No more instructions"
  exec' _ _ _ (instr:_) _ = Left $ "Error: Unimplemented or unknown instruction: " ++ show instr


execCall :: ArgCount -> Val -> Env -> Args -> Stack -> Either String Stack
execCall _ (Op op) _ args deeperStack = do
    resultStack <- execOp op args
    return (resultStack ++ deeperStack)
execCall argcount (Func program) globalEnv args deeperStack = do
    when (length args /= argcount) $
        Left $ "Error: The function expects " ++ show argcount ++ " arguments, but received " ++ show (length args)
    let argsForNewCall = reverse args
    -- The new function call gets a fresh, empty local scope.
    res <- exec argsForNewCall globalEnv program []
    return (res : deeperStack)
execCall _ val _ _ _ = Left $ "Error: Attempted to call a non-functional value: " ++ show val


execOp :: Op -> Stack -> Either String Stack
execOp Add (Num a:Num b:stack) = return $ Num (a + b):stack
execOp Sub (Num a:Num b:stack) = return $ Num (b - a):stack
execOp Div (Num a:Num b:stack)
    | a == 0    = Left "Error: Division by zero"
    | otherwise = return $ Num (b `div` a):stack
execOp Mul (Num a:Num b:stack) = return $ Num (a * b):stack
execOp Lt (Num a:Num b:stack) = return $ Bool (b < a):stack
execOp Lt (Bool a:Bool b:stack) = return $ Bool (b < a):stack
execOp Eq (Num a:Num b:stack) = return $ Bool (a == b):stack
execOp Eq (Bool a:Bool b:stack) = return $ Bool (a == b):stack
execOp Neg (Num a:stack) = return $ Num (-a):stack
execOp Neg _ = Left "Error: Neg expects one Num argument"
execOp Gt (Num a:Num b:stack) = return $ Bool (b > a):stack
execOp Gt (Bool a:Bool b:stack) = return $ Bool (b > a):stack
execOp Not (Bool a:stack) = return $ Bool (not a):stack
execOp Not _ = Left "Error: Not expects one Bool argument"
execOp And (Bool a:Bool b:stack) = return $ Bool (a && b):stack
execOp And _ = Left "Error: And expects two Bool arguments"
execOp Or (Bool a:Bool b:stack) = return $ Bool (a || b):stack
execOp Or _ = Left "Error: Or expects two Bool arguments"
execOp Xor (Bool a:Bool b:stack) = return $ Bool (a /= b):stack
execOp Xor _ = Left "Error: Xor expects two Bool arguments"
execOp Cons (v:(List vs):stack) = return $ List (v:vs):stack
execOp Cons _ = Left "Error: Cons expects an element and a list on the stack"
execOp Car ((List (v:_)):stack) = return $ v:stack
execOp Car (List []:_) = Left "Error: Car cannot be called on an empty list"
execOp Car _ = Left "Error: Car expects a list on the stack"
execOp Cdr ((List (_:vs)):stack) = return $ List vs:stack
execOp Cdr (List []:_) = Left "Error: Cdr cannot be called on an empty list"
execOp Cdr _ = Left "Error: Cdr expects a list on the stack"
execOp EmptyList stack = return $ List []:stack
execOp op []  = Left $ "Error: " ++ show op ++ " expects arguments, but the stack is empty."
execOp op [_] = Left $ "Error: " ++ show op ++ " expects at least two arguments, but received only one."
execOp op _   = Left $ "Error: " ++ show op ++ ", incorrect argument types."
