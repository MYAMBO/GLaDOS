module VmExec where

import Control.Monad (when)
import Op

exec :: Args -> Env -> Program -> Stack -> Either String Val
exec args env = exec' where
  exec' (Call argcount:program) stack = do
      when (length stack < argcount + 1) $
          Left $ "Error: Call expects " ++ show argcount ++ " arguments and a function on the stack"
      let (argsPopped, funcAndRest) = splitAt argcount stack
      case funcAndRest of
          (func:deeperStack) -> do
              newStack <- execCall argcount func env argsPopped deeperStack
              exec' program newStack
          [] -> Left "Error: Corrupted stack, function not found after arguments"
  exec' (TailCall argcount:program) stack = do
      when (length stack < argcount + 1) $
          Left $ "Error: Call expects " ++ show argcount ++ " arguments and a function on the stack"
      let (argsPopped, funcAndRest) = splitAt argcount stack
      case funcAndRest of
          (Func newProgram:deeperStack) -> do
              when (length argsPopped /= argcount) $
                  Left $ "Error: The function expects " ++ show argcount ++ " arguments, but received " ++ show (length argsPopped)
              let newArgs = reverse argsPopped
              exec newArgs env newProgram deeperStack
          (Op op:deeperStack) ->
              execCall argcount (Op op) env argsPopped deeperStack >>= \newStack -> exec' program newStack
          (val:_) -> Left $ "Error: Attempted to call a non-functional value: " ++ show val
          [] -> Left "Error: Corrupted stack, function not found after arguments"
  exec' (PushFromArgs n:program) stack = exec' program ((args !! n):stack)
  exec' (PushFromEnv s:program) stack = case lookup s env of
      Just v  -> exec' program (v:stack)
      Nothing -> Left $ "Error: variable " ++ s ++ " not found in environment"
  exec' (Push a:program) stack = exec' program (a:stack)
  exec' (Pop:program) (_:stack) = exec' program stack
  exec' (Pop:_) [] = Left "Error: Pop: Stack is empty"
  exec' (Return:_) (a:_) = return a
  exec' (Return:_) [] = Left "Error: Ret: Stack is empty"
  exec' (Jump n:program) stack = exec' (drop n program) stack
  exec' (JumpIfFalse n:program) (Bool False:stack) = exec' (drop n program) stack
  exec' (JumpIfFalse _:program) (Bool True:stack) = exec' program stack
  exec' (JumpIfFalse _:_) (val:_) = Left $ "Error: JumpIfFalse expects a Boolean value, but found " ++ show val
  exec' (JumpIfFalse _:_) [] = Left "Error: JumpIfFalse expects a value on the stack"
  exec' [] _ = Left "Error: No more instructions"
  exec' (instr:_) _ = Left $ "Error: Unimplemented or unknown instruction: " ++ show instr


execCall :: ArgCount -> Val -> Env -> Args -> Stack -> Either String Stack
execCall _ (Op op) _ args deeperStack = do
    resultStack <- execOp op args
    return (resultStack ++ deeperStack)
execCall argcount (Func program) env args deeperStack = do
    when (length args /= argcount) $
        Left $ "Error: The function expects " ++ show argcount ++ " arguments, but received " ++ show (length args)
    let argsForNewCall = reverse args
    res <- exec argsForNewCall env program []
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
execOp op []  = Left $ "Error: " ++ show op ++ " expects arguments, but the stack is empty."
execOp op [_] = Left $ "Error: " ++ show op ++ " expects at least two arguments, but received only one."
execOp op _   = Left $ "Error: " ++ show op ++ ", incorrect argument types."
