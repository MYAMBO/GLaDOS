{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Compiler
-}

module Compiler (compile) where

import Data (Ast(..), Builtins(..), VariableAst(..))
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State (StateT, gets, modify, runStateT, lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftEither)

-- =============================================================================
-- COMPILER STATE AND MONAD
-- =============================================================================

data CompilerState = CompilerState {
    csBuilder    :: BB.Builder,
    csLocalScope :: Set.Set String,
    csArgScope   :: Map.Map String Int
}

initialState :: CompilerState
initialState = CompilerState { csBuilder = mempty, csLocalScope = Set.empty, csArgScope = Map.empty }

type Compiler = StateT CompilerState (ExceptT String IO)

runCompiler :: Compiler a -> CompilerState -> IO (Either String (a, CompilerState))
runCompiler comp st = runExceptT $ runStateT comp st

-- =============================================================================
-- BYTECODE DEFINITIONS
-- =============================================================================

opcodes :: [(String, Word8)]
opcodes = [ ("Push", 0x01), ("Pop", 0x02), ("Return", 0x03), ("Jump", 0x04),
            ("JumpIfFalse", 0x05), ("Call", 0x06), ("PushFromArgs", 0x08),
            ("PushFromEnv", 0x09), ("Define", 0x0A), ("Assign", 0x0B) ]

valTags :: [(String, Word8)]
valTags = [ ("BoolVal", 0x01), ("Op", 0x02), ("Func", 0x03), ("Int8Val", 0x10),
            ("Int16Val", 0x11), ("Int32Val", 0x12), ("Int64Val", 0x13),
            ("Word8Val", 0x20), ("Word16Val", 0x21), ("Word32Val", 0x22),
            ("Word64Val", 0x23), ("FltVal", 0x30), ("DblVal", 0x31) ]

opOpcodes :: [(Builtins, Word8)]
opOpcodes = [ (Add, 0x01), (Subtract, 0x02), (Multiply, 0x03), (Divide, 0x04),
              (Modulo, 0x05), (Neg, 0x06), (Equal, 0x07), (LessThan, 0x08),
              (GreaterThan, 0x09), (Not, 0x0A), (And, 0x0B), (Or, 0x0C), (Xor, 0x0D) ]

-- =============================================================================
-- BYTECODE EMITTER HELPERS
-- =============================================================================

emit :: BB.Builder -> Compiler ()
emit b = modify $ \s -> s { csBuilder = csBuilder s <> b }
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
emitPushVal :: VariableAst -> Compiler ()
emitPushVal val = emitInstruction "Push" $ do
    case val of
        Int8 v   -> findOpcode "Int8Val" valTags "value tag" >>= emitB >> emit (BB.int8 v)
        Int16 v  -> findOpcode "Int16Val" valTags "value tag" >>= emitB >> emit (BB.int16BE v)
        Int32 v  -> findOpcode "Int32Val" valTags "value tag" >>= emitB >> emit (BB.int32BE v)
        Int64 v  -> findOpcode "Int64Val" valTags "value tag" >>= emitB >> emit (BB.int64BE v)
        UInt8 v  -> findOpcode "Word8Val" valTags "value tag" >>= emitB >> emit (BB.word8 v)
        UInt16 v -> findOpcode "Word16Val" valTags "value tag" >>= emitB >> emit (BB.word16BE v)
        UInt32 v -> findOpcode "Word32Val" valTags "value tag" >>= emitB >> emit (BB.word32BE v)
        UInt64 v -> findOpcode "Word64Val" valTags "value tag" >>= emitB >> emit (BB.word64BE v)
        Float v  -> findOpcode "FltVal" valTags "value tag" >>= emitB >> emit (BB.floatBE v)
        Double v -> findOpcode "DblVal" valTags "value tag" >>= emitB >> emit (BB.doubleBE v)
        Bool v   -> findOpcode "BoolVal" valTags "value tag" >>= emitB >> emitB (if v then 1 else 0)
        _ -> throwError $ "Compiler Error: Pushing string literals is not directly supported."
emitBuiltinOp :: Builtins -> Compiler ()
emitBuiltinOp builtin = emitInstruction "Push" $ do
    opTag <- findOpcode "Op" valTags "value tag"
    opCode <- findOpcode builtin opOpcodes "builtin operation"
    emitB opTag
    emitB opCode

-- =============================================================================
-- MAIN COMPILER LOGIC
-- =============================================================================

compileTransformedOp :: Ast -> Ast -> Builtins -> Builtins -> Compiler ()
compileTransformedOp argA argB innerOp outerOp = do
    emitBuiltinOp outerOp; emitBuiltinOp innerOp
    compileAst argA; compileAst argB
    emitInstruction "Call" (emitI32 2)
    emitInstruction "Call" (emitI32 1)

compileAst :: Ast -> Compiler ()
compileAst (Define name (Lambda args (Symbol _) body)) = do
    isDefined <- gets (Set.member name . csLocalScope)
    if isDefined then throwError $ "Function redefinition: '" ++ name ++ "'"
    else do
        (functionBytecode, _) <- compileLambda args body
        emitInstruction "Push" $ do
            funcTag <- findOpcode "Func" valTags "value tag"
            emitB funcTag
            emitI32 (fromIntegral $ BL.length functionBytecode)
            emit (BB.lazyByteString functionBytecode)
        emitInstruction "Define" (emitString name)
        modify $ \s -> s { csLocalScope = Set.insert name (csLocalScope s) }

compileAst (Define name ast) = do
    isDefined <- gets (Set.member name . csLocalScope)
    if isDefined then throwError $ "Variable redefinition: '" ++ name ++ "'"
    else do
        compileAst ast
        emitInstruction "Define" (emitString name)
        modify $ \s -> s { csLocalScope = Set.insert name (csLocalScope s) }

compileAst (Literal val) = emitPushVal val

compileAst (Symbol name) = do
    argIndex <- gets (Map.lookup name . csArgScope)
    case argIndex of
        Just index -> emitInstruction "PushFromArgs" (emitI32 index)
        Nothing -> do
            isLocal <- gets (Set.member name . csLocalScope)
            if isLocal then emitInstruction "PushFromEnv" (emitString name)
            else throwError $ "Undefined variable: '" ++ name ++ "'"

compileAst (Call func argsAst) = do
    compileAst func
    mapM_ compileAst argsAst
    emitInstruction "Call" (emitI32 (length argsAst))

compileAst (BinOp op args) = do
    let (requiredArity, argNodes) = case op of
            Not -> (1, args); Neg -> (1, args); _ -> (2, args)
    if length args /= requiredArity then throwError $ "Operation '" ++ show op ++ "' expects " ++ show requiredArity ++ " args."
    else
        case op of
            NotEqual           -> compileTransformedOp (head argNodes) (argNodes !! 1) Equal Not
            LessThanOrEqual    -> compileTransformedOp (head argNodes) (argNodes !! 1) GreaterThan Not
            GreaterThanOrEqual -> compileTransformedOp (head argNodes) (argNodes !! 1) LessThan Not
            _ -> do
                emitBuiltinOp op
                mapM_ compileAst argNodes
                emitInstruction "Call" (emitI32 requiredArity)

compileAst (If cond thenBranch elseBranch) = do
    compileAst cond
    thenBuilder <- compileBranch thenBranch
    let thenSize = fromIntegral $ BL.length $ BB.toLazyByteString thenBuilder
    elseBuilder <- compileBranch elseBranch
    let elseSize = fromIntegral $ BL.length $ BB.toLazyByteString elseBuilder
    emitInstruction "JumpIfFalse" (emitI32 (thenSize + 5))
    emit thenBuilder
    emitInstruction "Jump" (emitI32 elseSize)
    emit elseBuilder

compileAst ast = throwError $ "AST node not yet supported: " ++ show ast

compileLambda :: [Ast] -> Ast -> Compiler (BL.ByteString, [String])
compileLambda args body = do
    savedScope <- gets csLocalScope
    let extractArgName (Var _ name) = Right name
        extractArgName _ = Left "Invalid AST for lambda argument; expected Var _ name."
    argNames <- liftEither $ mapM extractArgName args
    let argMap = Map.fromList $ zip argNames [0..]
    let lambdaLocalScope = foldr Set.insert savedScope argNames
    let tempState = CompilerState mempty lambdaLocalScope argMap
    let bodyCompiler = compileAst body >> (findOpcode "Return" opcodes "instruction" >>= emitB)
    result <- lift $ lift $ runCompiler bodyCompiler tempState
    case result of
        Left err -> throwError $ "Error compiling lambda body: " ++ err
        Right (_, finalState) ->
            return (BB.toLazyByteString $ csBuilder finalState, argNames)

compileBranch :: Ast -> Compiler BB.Builder
compileBranch branch = do
    savedScope <- gets csLocalScope
    savedArgScope <- gets csArgScope
    let tempState = CompilerState mempty savedScope savedArgScope
    result <- lift $ lift $ runCompiler (compileAst branch) tempState
    case result of
        Right (_, finalState) -> return $ csBuilder finalState
        Left err -> throwError $ "Error in branch compilation: " ++ err

compileSequence :: [Ast] -> Compiler ()
compileSequence [] = return ()
compileSequence [lastAst] = compileAst lastAst
compileSequence (ast:rest) = do
    compileAst ast
    case ast of
        Define _ _ -> return ()
        _          -> findOpcode "Pop" opcodes "instruction" >>= emitB
    compileSequence rest

-- =============================================================================
-- TOP-LEVEL COMPILER FUNCTION
-- =============================================================================

compile :: [Ast] -> IO (Either String BL.ByteString)
compile asts = do
    let compileProg = compileSequence asts >> findOpcode "Return" opcodes "instruction" >>= emitB
    result <- runCompiler compileProg initialState
    case result of
        Left err -> return $ Left err
        Right (_, finalState) -> do
            let header = BB.word32BE 0x42414B41 <> BB.word32BE 2
            let globalEnv = BB.word32BE 0
            let codeBody = csBuilder finalState
            let finalBytecode = BB.toLazyByteString $ header <> globalEnv <> codeBody
            return $ Right finalBytecode
