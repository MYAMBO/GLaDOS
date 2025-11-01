{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- BytecodeConvertor
-}

module BytecodeConvertor where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Binary.Get
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Char (ord, chr)

------------- Unsigned Int to Bytecode ----------------------------

unsignedInt64ToBytecode :: Int -> BL.ByteString
unsignedInt64ToBytecode n =
  let word = fromIntegral n :: Word64
  in BB.toLazyByteString (BB.word64BE word)

unsignedInt32ToBytecode :: Int -> BL.ByteString
unsignedInt32ToBytecode n =
  let word = fromIntegral n :: Word32
  in BB.toLazyByteString (BB.word32BE word)

unsignedInt16ToBytecode :: Int -> BL.ByteString
unsignedInt16ToBytecode n =
  let word = fromIntegral n :: Word16
  in BB.toLazyByteString (BB.word16BE word)

unsignedInt8ToBytecode :: Int -> BL.ByteString
unsignedInt8ToBytecode n =
  let word = fromIntegral n :: Word8
  in BB.toLazyByteString (BB.word8 word)

------------- Bytecode to unsigned Int ----------------------------

bytecodeToUnsignedInt64 :: BL.ByteString -> Int
bytecodeToUnsignedInt64 bs =
  fromIntegral (runGet getWord64be bs :: Word64)

bytecodeToUnsignedInt32 :: BL.ByteString -> Int
bytecodeToUnsignedInt32 bs =
  fromIntegral (runGet getWord32be bs :: Word32)

bytecodeToUnsignedInt16 :: BL.ByteString -> Int
bytecodeToUnsignedInt16 bs =
  fromIntegral (runGet getWord16be bs :: Word16)

bytecodeToUnsignedInt8 :: BL.ByteString -> Int
bytecodeToUnsignedInt8 bs =
  fromIntegral (runGet getWord8 bs :: Word8)

------------- Signed Int to Bytecode ----------------------------

int64ToBytecode :: Int -> BL.ByteString
int64ToBytecode n =
  let w = fromIntegral n :: Int64
  in BB.toLazyByteString (BB.int64BE w)

int32ToBytecode :: Int -> BL.ByteString
int32ToBytecode n =
  let w = fromIntegral n :: Int32
  in BB.toLazyByteString (BB.int32BE w)

int16ToBytecode :: Int -> BL.ByteString
int16ToBytecode n =
  let w = fromIntegral n :: Int16
  in BB.toLazyByteString (BB.int16BE w)

int8ToBytecode :: Int -> BL.ByteString
int8ToBytecode n =
  let w = fromIntegral n :: Int8
  in BB.toLazyByteString (BB.int8 w)

------------- Bytecode to Signed Int ----------------------------

bytecodeToInt64 :: BL.ByteString -> Int
bytecodeToInt64 bs =
  fromIntegral (runGet getInt64be bs :: Int64)

bytecodeToInt32 :: BL.ByteString -> Int
bytecodeToInt32 bs =
  fromIntegral (runGet getInt32be bs :: Int32)

bytecodeToInt16 :: BL.ByteString -> Int
bytecodeToInt16 bs =
  fromIntegral (runGet getInt16be bs :: Int16)

bytecodeToInt8 :: BL.ByteString -> Int
bytecodeToInt8 bs =
  fromIntegral (runGet getInt8 bs :: Int8)

--------------- Bool / Bytecode -----------------------------------------------

boolToBytecode :: Bool -> BL.ByteString
boolToBytecode value =
  BB.toLazyByteString (BB.word8 (fromIntegral (fromEnum value)))

bytecodeToBool :: BL.ByteString -> Bool
bytecodeToBool = (/= 0) . BL.head

--------------- Char / Bytecode -----------------------------------------------

charToBytecode :: Char -> BB.Builder
charToBytecode c = BB.word8 (fromIntegral (ord c))

bytecodeToChar :: BL.ByteString -> Char
bytecodeToChar bs
    | BL.length bs /= 1 = error "ByteString must be exactly 1 byte"
    | otherwise =
        let byte = BL.head bs
        in chr (fromIntegral byte)
