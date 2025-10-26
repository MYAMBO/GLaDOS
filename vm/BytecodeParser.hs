{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description: BytecodeParser
-}

module BytecodeParser (parseAndExec) where

import Op
import VmExec (exec)

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Control.Monad (replicateM, when)

parseAndExec :: FilePath -> IO ()
parseAndExec path = do
    content <- B.readFile path
    case runGetOrFail parseBytecodeFile content of
        Left (_, _, errMsg) -> putStrLn $ "Parsing error: " ++ errMsg
        Right (_, _, (env, mainProgram)) ->
            case exec [] env mainProgram [] of
                Left err -> putStrLn err
                Right val -> putStrLn $ show val

parseBytecodeFile :: Get (Env, Program)
parseBytecodeFile = do
    magic <- getWord32be
    when (magic /= 0x42414B41) $ fail "Invalid Magic number"
    _ <- getWord32be -- Skip version number for now
    env <- getEnv
    mainProgram <- getProgram
    return (env, mainProgram)

getEnv :: Get Env
getEnv = do
    count <- getWord32be
    replicateM (fromIntegral count) getEnvEntry

getEnvEntry :: Get (String, Val)
getEnvEntry = do
    keyLen <- fromIntegral <$> getWord32be
    keyBytes <- getLazyByteString keyLen
    val <- getVal
    -- Convert lazy bytestring to a standard String
    let key = map (toEnum . fromIntegral) (B.unpack keyBytes)
    return (key, val)

getProgram :: Get Program
getProgram = do
    empty <- isEmpty
    if empty
        then return []
        else (:) <$> getInstruction <*> getProgram

getInstruction :: Get Instruction
getInstruction = getWord8 >>= \opcode -> case opcode of
    0x01 -> Push <$> getVal
    0x02 -> return Pop
    0x03 -> return Return
    0x04 -> Jump . fromIntegral <$> getWord32be
    0x05 -> JumpIfFalse . fromIntegral <$> getWord32be
    0x06 -> Call . fromIntegral <$> getWord32be
    0x07 -> TailCall . fromIntegral <$> getWord32be
    0x08 -> PushFromArgs . fromIntegral <$> getWord32be
    0x09 -> PushFromEnv <$> getString
    0x0A -> Define <$> getString
    0x0B -> Assign <$> getString
    _    -> fail $ "Unknown instruction: " ++ show opcode
    where
      getString = do
        len <- fromIntegral <$> getWord32be
        strBytes <- getLazyByteString len
        return $ map (toEnum . fromIntegral) (B.unpack strBytes)


getVal :: Get Val
getVal = getWord8 >>= \tag -> case tag of
    -- Structural types
    0x01 -> BoolVal . (/= 0) <$> getWord8
    0x02 -> Op <$> getOp
    0x03 -> do
        instrCount <- getWord32be
        Func <$> replicateM (fromIntegral instrCount) getInstruction
    -- Signed integer types
    0x10 -> Int8Val <$> getInt8
    0x11 -> Int16Val <$> getInt16be
    0x12 -> Int32Val <$> getInt32be
    0x13 -> Int64Val <$> getInt64be
    -- Unsigned integer types
    0x20 -> Word8Val <$> getWord8
    0x21 -> Word16Val <$> getWord16be
    0x22 -> Word32Val <$> getWord32be
    0x23 -> Word64Val <$> getWord64be
    -- Floating-point types
    0x30 -> FltVal <$> getFloatbe
    0x31 -> DblVal <$> getDoublebe
    _    -> fail $ "Unknown value type tag: " ++ show tag

getOp :: Get Op
getOp = getWord8 >>= \opcode -> case opcode of
    0x01 -> return Add; 0x02 -> return Sub; 0x03 -> return Mul
    0x04 -> return Div; 0x05 -> return Mod; 0x06 -> return Neg
    0x07 -> return Eq;  0x08 -> return Lt;  0x09 -> return Gt
    0x0A -> return Not; 0x0B -> return And; 0x0C -> return Or
    0x0D -> return Xor; 0x0E -> return Cons; 0x0F -> return Car
    0x10 -> return Cdr; 0x11 -> return EmptyList
    _    -> fail $ "Unknown operation: " ++ show opcode
