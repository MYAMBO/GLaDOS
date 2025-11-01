{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Compiler
-}

module Compiler (compile) where

import Data.Word (Word8)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import DataTypes (Ast(..), Builtins(..), VariableAst(..))
import Control.Monad (forM_, unless)
import Control.Monad.State (StateT, gets, modify, runStateT, lift, get)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftEither)
import Data.List (partition)

data CompilerState = CompilerState {
    csBuilder          :: BB.Builder,
    csGlobalEnvBuilder :: BB.Builder,
    csGlobalEntryCount :: Int,
    csLocalScope       :: Set.Set String,
    csArgScope         :: Map.Map String Int
}

initialState :: CompilerState
initialState = CompilerState {
    csBuilder = mempty,
    csGlobalEnvBuilder = mempty,
    csGlobalEntryCount = 0,
    csLocalScope = Set.empty,
    csArgScope = Map.empty
}

type Compiler = StateT CompilerState (ExceptT String IO)

runCompiler :: Compiler a -> CompilerState -> IO (Either String (a, CompilerState))
runCompiler comp st = runExceptT $ runStateT comp st

opcodes :: [(String, Word8)]
opcodes = [ ("Push", 0x01), ("Pop", 0x02), ("Return", 0x03), ("Jump", 0x04),
            ("JumpIfFalse", 0x05), ("Call", 0x06), ("TailCall", 0x07), ("PushFromArgs", 0x08),
            ("PushFromEnv", 0x09), ("Define", 0x0A), ("Assign", 0x0B) ]

valTags :: [(String, Word8)]
valTags = [ ("BoolVal", 0x01), ("Op", 0x02), ("Func", 0x03), ("Int8Val", 0x10),
            ("Int16Val", 0x11), ("Int32Val", 0x12), ("Int64Val", 0x13),
            ("Word8Val", 0x20), ("Word16Val", 0x21), ("Word32Val", 0x22),
            ("Word64Val", 0x23), ("FltVal", 0x30), ("DblVal", 0x31) ]

opOpcodes :: [(Builtins, Word8)]
opOpcodes = [ (Add, 0x01), (Subtract, 0x02), (Multiply, 0x03), (Divide, 0x04),
              (Modulo, 0x05), (Neg, 0x06), (Equal, 0x07), (LessThan, 0x08),
              (GreaterThan, 0x09), (Not, 0x0A), (And, 0x0B), (Or, 0x0C), (Xor, 0x0D),
              (Cons, 0x0E), (Car, 0x0F), (Cdr, 0x10), (EmptyList, 0x11) ]

emit :: BB.Builder -> Compiler ()
emit b = modify $ \s -> s { csBuilder = csBuilder s <> b }
emitGlobal :: BB.Builder -> Compiler ()
emitGlobal b = modify $ \s -> s { csGlobalEnvBuilder = csGlobalEnvBuilder s <> b }
emitB :: Word8 -> Compiler ()
emitB = emit . BB.word8
emitI32 :: Int -> Compiler ()
emitI32 n = emit $ BB.int32BE (fromIntegral n)
emitString :: String -> Compiler ()
emitString s = do
    let bs = BB.stringUtf8 s
    emitI32 (fromIntegral $ BL.length $ BB.toLazyByteString bs)
    emit bs
findOpcode :: (Eq a, Show a) => a -> [(a, Word8)] -> String -> Compiler Word8
findOpcode key table context = case lookup key table of
    Just code -> return code
    Nothing   -> throwError $ "Compiler Internal Error: No opcode for " ++ context ++ ": " ++ show key
emitInstruction :: String -> Compiler () -> Compiler ()
emitInstruction name emitArgs = do
    opcode <- findOpcode name opcodes "instruction"
    emitB opcode
    emitArgs
emitValueBuilder :: VariableAst -> Compiler BB.Builder
emitValueBuilder val =
    case val of
        Int8 v    -> tagBuilder "Int8Val" (BB.int8 v)
        Int16 v   -> tagBuilder "Int16Val" (BB.int16BE v)
        Int32 v   -> tagBuilder "Int32Val" (BB.int32BE v)
        Int64 v   -> tagBuilder "Int64Val" (BB.int64BE v)
        UInt8 v   -> tagBuilder "Word8Val" (BB.word8 v)
        UInt16 v  -> tagBuilder "Word16Val" (BB.word16BE v)
        UInt32 v  -> tagBuilder "Word32Val" (BB.word32BE v)
        UInt64 v  -> tagBuilder "Word64Val" (BB.word64BE v)
        Float v   -> tagBuilder "FltVal" (BB.floatBE v)
        Double v  -> tagBuilder "DblVal" (BB.doubleBE v)
        Bool v    -> tagBuilder "BoolVal" (BB.word8 (if v then 1 else 0))
        _ -> throwError $ "Compiler Error: Global definition for this literal type is not supported."
    where
      tagBuilder name payload = do
        tag <- findOpcode name valTags "value tag"
        return $ BB.word8 tag <> payload
emitPushVal :: VariableAst -> Compiler ()
emitPushVal val = do
    builder <- emitValueBuilder val
    emitInstruction "Push" (emit builder)
emitBuiltinOp :: Builtins -> Compiler ()
emitBuiltinOp builtin = emitInstruction "Push" $ do
    opTag <- findOpcode "Op" valTags "value tag"
    opCode <- findOpcode builtin opOpcodes "builtin operation"
    emitB opTag
    emitB opCode

compileGlobalDefine :: String -> Ast -> Compiler ()
compileGlobalDefine name ast = do
    isDefined <- gets (Set.member name . csLocalScope)
    if isDefined then throwError $ "Redefinition of global identifier: '" ++ name ++ "'"
    else do
        modify $ \s -> s { csLocalScope = Set.insert name (csLocalScope s) }
        valueBuilder <- case ast of
            (Lambda args (Symbol _) body) -> do
                (functionBytecode, _) <- compileLambda args body
                funcTag <- findOpcode "Func" valTags "value tag"
                let lenBuilder = BB.int32BE (fromIntegral $ BL.length functionBytecode)
                return $ BB.word8 funcTag <> lenBuilder <> BB.lazyByteString functionBytecode
            (Literal val) -> emitValueBuilder val
            (Var val _) -> emitValueBuilder val
            _ -> throwError $ "Global definitions must be functions or constant literals. Found: " ++ show ast
        emitGlobal $ BB.int32BE (fromIntegral $ length name) <> BB.stringUtf8 name <> valueBuilder
        modify $ \s -> s { csGlobalEntryCount = csGlobalEntryCount s + 1 }

compileTransformedOp :: Ast -> Ast -> Builtins -> Builtins -> Compiler ()
compileTransformedOp argA argB innerOp outerOp = do
    emitBuiltinOp outerOp
    compileAst (BinOp innerOp [argA, argB]) False
    emitInstruction "Call" (emitI32 1)

type TailContext = Bool
type TailAstCompiler = Ast -> TailContext -> Compiler ()

compileAst :: TailAstCompiler
compileAst (Define name ast) _ = compileGlobalDefine name ast

compileAst (Literal val) _ = emitPushVal val

compileAst (Symbol name) _ = do
    argIndex <- gets (Map.lookup name . csArgScope)
    case argIndex of
        Just index -> emitInstruction "PushFromArgs" (emitI32 index)
        Nothing -> do
            isLocal <- gets (Set.member name . csLocalScope)
            if isLocal then emitInstruction "PushFromEnv" (emitString name)
            else throwError $ "Undefined variable: '" ++ name ++ "'"

compileAst (Call func argsAst) inTailContext = do
    compileAst func False
    mapM_ (\arg -> compileAst arg False) argsAst
    if inTailContext
        then emitInstruction "TailCall" (emitI32 (length argsAst))
        else emitInstruction "Call" (emitI32 (length argsAst))

compileAst (BinOp op args) inTailContext = do
    let (requiredArity, argNodes) = case op of
            Not -> (1, args); Neg -> (1, args); Car -> (1, args); Cdr -> (1, args)
            _ -> (2, args)
    if length args /= requiredArity then throwError $ "Operation '" ++ show op ++ "' expects " ++ show requiredArity ++ " args, but got " ++ show (length args)
    else
        case op of
            NotEqual           -> compileTransformedOp (head argNodes) (argNodes !! 1) Equal Not
            LessThanOrEqual    -> compileTransformedOp (head argNodes) (argNodes !! 1) GreaterThan Not
            GreaterThanOrEqual -> compileTransformedOp (head argNodes) (argNodes !! 1) LessThan Not
            _ -> do
                emitBuiltinOp op
                mapM_ (\arg -> compileAst arg False) argNodes
                if inTailContext
                    then emitInstruction "TailCall" (emitI32 requiredArity)
                    else emitInstruction "Call" (emitI32 requiredArity)

compileAst (If cond thenBranch elseBranch) inTailContext = do
    compileAst cond False
    thenBuilder <- compileBranch thenBranch inTailContext
    let thenSize = fromIntegral $ BL.length $ BB.toLazyByteString thenBuilder
    elseBuilder <- compileBranch elseBranch inTailContext
    let elseSize = fromIntegral $ BL.length $ BB.toLazyByteString elseBuilder
    emitInstruction "JumpIfFalse" (emitI32 (thenSize + 5))
    emit thenBuilder
    emitInstruction "Jump" (emitI32 elseSize)
    emit elseBuilder

compileAst (LiteralList _ elements) inTailContext = do
    emitBuiltinOp EmptyList
    emitInstruction "Call" (emitI32 0)
    forM_ (reverse elements) compileListElement
    where
      compileListElement element = do
        emitBuiltinOp Cons
        compileAst element False
        emitInstruction "Call" (emitI32 2)

compileAst (List []) _ = return ()

compileAst (List [singleAst]) inTailContext = compileAst singleAst inTailContext

compileAst (List statements) inTailContext = do
    compileStatements statements inTailContext
  where
    compileStatements [] _ = return ()
    compileStatements [lastStmt] True = compileAst lastStmt True
    compileStatements [lastStmt] False = compileAst lastStmt False
    compileStatements (stmt:rest) inTailContext = do
        case stmt of
            Define varName varValueAst -> do
                compileAst varValueAst False
                modify $ \s -> s { csLocalScope = Set.insert varName (csLocalScope s) }
                emitInstruction "Define" (emitString varName)
            _ -> do
                compileAst stmt False
                emitInstruction "Pop" (return ())
        compileStatements rest inTailContext

compileAst ast _ = throwError $ "AST node not yet supported in this context: " ++ show ast

compileLambda :: [Ast] -> Ast -> Compiler (BL.ByteString, [String])
compileLambda args body = do
    currentState <- get
    let extractArgName (Var _ name) = Right name
        extractArgName _ = Left "Invalid AST for lambda argument; expected Var _ name."
    argNames <- liftEither $ mapM extractArgName args
    let argMap = Map.fromList $ zip argNames [0..]
    let lambdaLocalScope = foldr Set.insert (csLocalScope currentState) argNames
    let tempState = currentState {
        csBuilder = mempty,
        csGlobalEnvBuilder = mempty,
        csLocalScope = lambdaLocalScope,
        csArgScope = argMap
    }
    let bodyCompiler = compileAst body True >> (findOpcode "Return" opcodes "instruction" >>= emitB)
    result <- lift $ lift $ runCompiler bodyCompiler tempState
    case result of
        Left err -> throwError $ "Error compiling lambda body: " ++ err
        Right (_, finalState) ->
            return (BB.toLazyByteString $ csBuilder finalState, argNames)

compileBranch :: Ast -> Bool -> Compiler BB.Builder
compileBranch branch inTailContext = do
    currentState <- get
    let tempState = currentState {
        csBuilder = mempty,
        csGlobalEnvBuilder = mempty
    }
    result <- lift $ lift $ runCompiler (compileAst branch inTailContext) tempState
    case result of
        Right (_, finalState) -> return $ csBuilder finalState
        Left err -> throwError $ "Error in branch compilation: " ++ err

isMainFunc :: Ast -> Bool
isMainFunc (Define "main" (Lambda {})) = True
isMainFunc _ = False

compile :: [Ast] -> IO (Either String BL.ByteString)
compile asts =
    if not (any isMainFunc asts)
    then return $ Left "Compilation failed: 'main' function is not defined."
    else do
        let initialStateForGlobals = initialState
        let globalCompiler = forM_ asts $ \ast ->
                case ast of
                    Define name defAst -> compileGlobalDefine name defAst
                    _ -> throwError "Top-level statements must be definitions."
        globalsResult <- runCompiler globalCompiler initialStateForGlobals
        case globalsResult of
            Left err -> return $ Left err
            Right (_, globalsState) -> do
                let entryPointCompiler = compileAst (Call (Symbol "main") []) False >> (findOpcode "Return" opcodes "instruction" >>= emitB)
                let entryPointState = initialState { csLocalScope = csLocalScope globalsState }
                entryPointResult <- runCompiler entryPointCompiler entryPointState
                case entryPointResult of
                    Left err -> return $ Left err
                    Right (_, entryPointFinalState) -> do
                        let header = BB.word32BE 0x42414B41 <> BB.word32BE 2
                        let globalCount = BB.int32BE (fromIntegral $ csGlobalEntryCount globalsState)
                        let globalEnv = csGlobalEnvBuilder globalsState
                        let codeBody = csBuilder entryPointFinalState
                        let finalBytecode = BB.toLazyByteString $ header <> globalCount <> globalEnv <> codeBody
                        return $ Right finalBytecode
