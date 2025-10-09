{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- BytecodeParser
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
        Left (_, _, errMsg) -> putStrLn $ "Parsing error : " ++ errMsg
        Right (_, _, (env, mainProgram)) -> do
--            putStrLn $ "Env: " ++ show env
--            putStrLn $ "Programme Principal: " ++ show mainProgram
            case exec [] env mainProgram [] of
                Left err -> putStrLn $ err
                Right val -> putStrLn $ show val

parseBytecodeFile :: Get (Env, Program)
parseBytecodeFile = do
    magic <- getWord32be
    when (magic /= 0x42414B41) $ fail "Invalid Magic number"
    _ <- getWord32be
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
    return (map (toEnum . fromIntegral) (B.unpack keyBytes), val)

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
    0x09 -> do
        len <- fromIntegral <$> getWord32be
        strBytes <- getLazyByteString len
        return $ PushFromEnv (map (toEnum . fromIntegral) (B.unpack strBytes))
    _    -> fail $ "Unknown instruction : " ++ show opcode

getVal :: Get Val
getVal = getWord8 >>= \tag -> case tag of
    0x01 -> Num . fromIntegral <$> getInt32be
    0x02 -> Bool . (/= 0) <$> getWord8
    0x03 -> Op <$> getOp
    0x04 -> do
        instrCount <- getWord32be
        Func <$> replicateM (fromIntegral instrCount) getInstruction
    _    -> fail $ "Value type not known : " ++ show tag

getOp :: Get Op
getOp = getWord8 >>= \opcode -> case opcode of
    0x01 -> return Add; 0x02 -> return Sub; 0x03 -> return Mul
    0x04 -> return Div; 0x05 -> return Mod; 0x06 -> return Neg
    0x07 -> return Eq;  0x08 -> return Lt;  0x09 -> return Gt
    0x0A -> return Not; 0x0B -> return And; 0x0C -> return Or
    0x0D -> return Xor
    _    -> fail $ "Unknown operation : " ++ show opcode
