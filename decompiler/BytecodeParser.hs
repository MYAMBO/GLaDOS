{-
-- EPITECH PROJECT, 2025
-- GLaDOS
-- File description:
-- Bytecode → Surface language (String)
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Decompiler
  ( decompileToString
  , writeDecompiled
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Bits ((.|.), shiftL)
import Data.Int  (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Char (chr, isPrint)
import Unsafe.Coerce (unsafeCoerce)

------------------------------------------------------------
-- Constants / Tags / Opcodes (v2)
------------------------------------------------------------

magicNumber, currentVersion :: Word32
magicNumber    = 0x42414B41 -- "BAKA"
currentVersion = 2

-- opcodes
opcPush, opcPop, opcReturn, opcJump, opcJumpIfFalse, opcCall,
  opcTailCall, opcPushFromArgs, opcPushFromEnv, opcDefine, opcAssign :: Word8
opcPush         = 0x01
opcPop          = 0x02
opcReturn       = 0x03
opcJump         = 0x04
opcJumpIfFalse  = 0x05
opcCall         = 0x06
opcTailCall     = 0x07
opcPushFromArgs = 0x08
opcPushFromEnv  = 0x09
opcDefine       = 0x0A
opcAssign       = 0x0B

-- value tags
tagBool, tagOp, tagFunc, tagI8, tagI16, tagI32, tagI64,
  tagW8, tagW16, tagW32, tagW64, tagFlt, tagDbl :: Word8
tagBool = 0x01
tagOp   = 0x02
tagFunc = 0x03
tagI8   = 0x10
tagI16  = 0x11
tagI32  = 0x12
tagI64  = 0x13
tagW8   = 0x20
tagW16  = 0x21
tagW32  = 0x22
tagW64  = 0x23
tagFlt  = 0x30
tagDbl  = 0x31

------------------------------------------------------------
-- Builtins → Ops
------------------------------------------------------------

data Op
  = Add | Sub | Mul | Div | Mod | Neg
  | Eq  | Lt  | Gt  | Not | And | Or | Xor
  | NE  | LE  | GE
  deriving (Eq, Show)

opFromByte :: Word8 -> Either String Op
opFromByte b = case b of
  0x01 -> Right Add; 0x02 -> Right Sub; 0x03 -> Right Mul
  0x04 -> Right Div; 0x05 -> Right Mod; 0x06 -> Right Neg
  0x07 -> Right Eq ; 0x08 -> Right Lt ; 0x09 -> Right Gt
  0x0A -> Right Not; 0x0B -> Right And; 0x0C -> Right Or
  0x0D -> Right Xor
  _    -> Left ("Unknown builtin: 0x" ++ hex2 b)

------------------------------------------------------------
-- Cursor + Readers (big-endian)
------------------------------------------------------------

data Cur = Cur { input :: !BL.ByteString, off :: !Int64 } deriving (Show)

atEnd :: Cur -> Bool
atEnd = BL.null . input

takeN :: Int64 -> Cur -> Either String (BL.ByteString, Cur)
takeN n (Cur bs i)
  | BL.length bs >= n = Right (BL.take n bs, Cur (BL.drop n bs) (i + n))
  | otherwise         = Left "Unexpected EOF"

getU8 :: Cur -> Either String (Word8, Cur)
getU8 c = do (b, c') <- takeN 1 c; pure (BL.head b, c')

getU16BE :: Cur -> Either String (Word16, Cur)
getU16BE c = do
  (bs, c') <- takeN 2 c
  case BL.unpack bs of
    [a,b] -> Right ((fromIntegral a `shiftL` 8) .|. fromIntegral b, c')
    _     -> Left "getU16BE: need 2 bytes"

getI16BE :: Cur -> Either String (Int, Cur)
getI16BE c = do (w, c') <- getU16BE c; pure (fromIntegral (fromIntegral w :: Int16), c')

getU32BE :: Cur -> Either String (Word32, Cur)
getU32BE c = do
  (bs, c') <- takeN 4 c
  case BL.unpack bs of
    [a,b,d,e] ->
      let w = (fromIntegral a `shiftL` 24)
           .|. (fromIntegral b `shiftL` 16)
           .|. (fromIntegral d `shiftL` 8)
           .|.  fromIntegral e
      in Right (w, c')
    _ -> Left "getU32BE: need 4 bytes"

getI32BE :: Cur -> Either String (Int, Cur)
getI32BE c = do (u, c') <- getU32BE c; pure (fromIntegral (fromIntegral u :: Int32), c')

getU64BE :: Cur -> Either String (Word64, Cur)
getU64BE c = do
  (bs, c') <- takeN 8 c
  pure (BL.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 bs, c')

getI64BE :: Cur -> Either String (Int, Cur)
getI64BE c = do (u, c') <- getU64BE c; pure (fromIntegral (fromIntegral u :: Int64), c')

getString :: Cur -> Either String ((String, Int64), Cur)
getString c = do
  (n, c1) <- getI32BE c
  if n < 0 then Left "Negative string length" else do
    (bs, c2) <- takeN (fromIntegral n) c1
    pure ((map (chr . fromIntegral) (BL.unpack bs), fromIntegral n), c2)

word32ToFloat  :: Word32 -> Float  ; word32ToFloat  = unsafeCoerce
word64ToDouble :: Word64 -> Double ; word64ToDouble = unsafeCoerce

hex2 :: Word8 -> String
hex2 w = let s="0123456789ABCDEF"; a=fromIntegral (w `div` 16); b=fromIntegral (w `mod` 16)
         in [s!!a, s!!b]

------------------------------------------------------------
-- AST
------------------------------------------------------------

data Lit = Lit { lty :: String, lshow :: String } deriving (Eq, Show)

data Expr
  = ELit Lit
  | EVar String
  | ECall Expr [Expr]
  | EBin Op [Expr]
  | EIf Expr Expr Expr
  deriving (Eq, Show)

data Top
  = TVar (Maybe String) String Expr
  | TFunc String [String] Expr   -- name, params, body
  | TExpr Expr
  deriving (Eq, Show)

lit :: String -> String -> Expr
lit t s = ELit (Lit t s)

inferTypeShallow :: Expr -> Maybe String
inferTypeShallow (ELit (Lit t _)) = Just t
inferTypeShallow _                = Nothing

-- Deep-ish type inference. We mostly care about Int32 for now.
inferExprTypeDeep :: Expr -> Maybe String
inferExprTypeDeep (ELit (Lit t _)) = Just t
inferExprTypeDeep (EVar _)         = Nothing
inferExprTypeDeep (ECall f args)   = unify (inferExprTypeDeep f : map inferExprTypeDeep args)
inferExprTypeDeep (EBin _ xs)      = unify (map inferExprTypeDeep xs)
inferExprTypeDeep (EIf _ t e)      = unify [inferExprTypeDeep t, inferExprTypeDeep e]

unify :: [Maybe String] -> Maybe String
unify = foldl pick Nothing
  where
    pick :: Maybe String -> Maybe String -> Maybe String
    pick Nothing y = y
    pick x Nothing = x
    pick (Just a) (Just b)
      | a == b    = Just a
      | otherwise = Just a  -- arbitrary but stable

-- rename variables in an Expr according to a mapping,
-- e.g. [("a0","n")] turns EVar "a0" into EVar "n"
substExpr :: [(String, String)] -> Expr -> Expr
substExpr env = go where
  go (EVar v) =
    case lookup v env of
      Just v' -> EVar v'
      Nothing -> EVar v
  go (ELit l)        = ELit l
  go (EBin o xs)     = EBin o (map go xs)
  go (ECall f args)  = ECall (go f) (map go args)
  go (EIf c t e)     = EIf (go c) (go t) (go e)

-- normalize negations etc.
normalize :: Expr -> Expr
normalize (EBin Not [x]) = case normalize x of
  EBin Eq [a,b] -> EBin NE [a,b]
  EBin Gt [a,b] -> EBin LE [a,b]
  EBin Lt [a,b] -> EBin GE [a,b]
  y             -> EBin Not [y]
normalize (EBin o xs)  = EBin o (map normalize xs)
normalize (EIf c t e)  = EIf (normalize c) (normalize t) (normalize e)
normalize (ECall f as) = ECall (normalize f) (map normalize as)
normalize x            = x

------------------------------------------------------------
-- Value / func body helpers
------------------------------------------------------------

skipAfterTag :: Word8 -> Cur -> Either String Cur
skipAfterTag tag c = case tag of
  t | t == tagBool -> snd <$> getU8 c
    | t == tagOp   -> snd <$> getU8 c
    | t == tagFunc -> do (n, c1) <- getI32BE c
                         (_, c2) <- takeN (fromIntegral n) c1
                         pure c2
    | t == tagI8   -> snd <$> getU8 c
    | t == tagI16  -> snd <$> getU16BE c
    | t == tagI32  -> snd <$> getU32BE c
    | t == tagI64  -> snd <$> getU64BE c
    | t == tagW8   -> snd <$> getU8 c
    | t == tagW16  -> snd <$> getU16BE c
    | t == tagW32  -> snd <$> getU32BE c
    | t == tagW64  -> snd <$> getU64BE c
    | t == tagFlt  -> snd <$> getU32BE c
    | t == tagDbl  -> snd <$> getU64BE c
    | otherwise    -> Left ("Unknown value tag: 0x" ++ hex2 tag)

readFuncValBody :: Cur -> Either String (BL.ByteString, Cur)
readFuncValBody c0 = do
  (n, c1) <- getI32BE c0
  takeN (fromIntegral n) c1

------------------------------------------------------------
-- Env parsing (functions live here in v2)
------------------------------------------------------------

parseEnvValAsTop :: String -> Cur -> Either String (Maybe Top, Cur)
parseEnvValAsTop name c0 = do
  (tag, c1) <- getU8 c0
  case tag of
    t | t == tagFunc -> do
          (bodyBytes, c2) <- readFuncValBody c1

          -- decode bodyBytes as expression
          let curBody = Cur bodyBytes 0
              bodyLen = BL.length bodyBytes
          (rawBodyExpr, rawArgc, _) <- deExpr curBody bodyLen

          -- choose pretty parameter names
          let paramNamesRaw = [ "a" ++ show i | i <- [0 .. rawArgc - 1] ]
              paramNamesPretty =
                case paramNamesRaw of
                  ["a0"] -> ["n"]   -- heuristic: single arg becomes "n"
                  xs     -> xs

              mapping = zip paramNamesRaw paramNamesPretty
              bodyExprPretty = substExpr mapping rawBodyExpr

          -- build final TFunc with pretty names
          pure (Just (TFunc name paramNamesPretty bodyExprPretty), c2)

      | t == tagOp -> do
          -- env op entries, not top-level funcs/vars we want to render
          (_b, c2) <- getU8 c1
          pure (Nothing, c2)

      | otherwise -> do
          c2 <- skipAfterTag t c1
          pure (Nothing, c2)

parseEnv :: Cur -> Either String ([Top], Cur)
parseEnv c0 = do
  (countW, c1) <- getU32BE c0
  let count = fromIntegral countW :: Int
  let loop 0 acc c = Right (reverse acc, c)
      loop n acc c = do
        ((key, _), cK) <- getString c
        (mt, cV) <- parseEnvValAsTop key cK
        loop (n - 1) (maybe acc (:acc) mt) cV
  loop count [] c1

------------------------------------------------------------
-- Expression decoder (byte-count semantics in jumps)
------------------------------------------------------------

data SI = SV Expr | SOP Op | SF BL.ByteString deriving (Show)
data Est = Est { stk :: [SI], maxArg :: Int } deriving (Show)
emptyEst :: Est; emptyEst = Est [] (-1)

pushE :: SI -> Est -> Est
pushE x e = e { stk = x : stk e }

popE :: Est -> Either String (SI, Est)
popE e = case stk e of
  []     -> Left "Stack underflow"
  (x:xs) -> Right (x, e { stk = xs })

popExpr :: Est -> Either String (Expr, Est)
popExpr e = do
  (si, e') <- popE e
  case si of
    SV v -> Right (v, e')
    _    -> Left "Expected expr"

deExpr :: Cur -> Int64 -> Either String (Expr, Int, Cur)
deExpr c0 lim = go c0 0 emptyEst
  where
    finish :: Est -> Either String (Expr, Int, Cur)
    finish e = case stk e of
      (SV v:_) -> Right (normalize v, max 0 (maxArg e + 1), Cur BL.empty (off c0 + lim))
      _        -> Left "Empty expression"

    bump :: Int64 -> Int64 -> Int64
    bump n m = m + n

    go :: Cur -> Int64 -> Est -> Either String (Expr, Int, Cur)
    go c used est
      | used >= lim || atEnd c = finish est
      | otherwise = do
          (op, c1) <- getU8 c
          case op of
            _ | op == opcPush -> do
                  (tag, c2) <- getU8 c1
                  case tag of
                    _ | tag == tagBool -> do
                          (b, c3) <- getU8 c2
                          go c3 (bump 3 used)
                             (pushE (SV (lit "Bool" (if b == 0 then "false" else "true"))) est)

                      | tag == tagOp -> do
                          (b, c3) <- getU8 c2
                          o <- opFromByte b
                          go c3 (bump 3 used) (pushE (SOP o) est)

                      | tag == tagFunc -> do
                          (bs, c3) <- readFuncValBody c2
                          go c3 (bump (1 + 4 + BL.length bs) used) (pushE (SF bs) est)

                      | tag == tagI8 -> do
                          (w, c3) <- getU8 c2
                          let i = fromIntegral (fromIntegral w :: Int8)
                              s = if i >= 32 && i <= 126 && isPrint (chr i)
                                    then ['\'', chr i, '\'']
                                    else show i
                          go c3 (bump 3 used) (pushE (SV (lit "Int8" s)) est)

                      | tag == tagI16 -> do
                          (i, c3) <- getI16BE c2
                          go c3 (bump 4 used) (pushE (SV (lit "Int16" (show i))) est)

                      | tag == tagI32 -> do
                          (i, c3) <- getI32BE c2
                          go c3 (bump 6 used) (pushE (SV (lit "Int32" (show i))) est)

                      | tag == tagI64 -> do
                          (i, c3) <- getI64BE c2
                          go c3 (bump 10 used) (pushE (SV (lit "Int64" (show i))) est)

                      | tag == tagW8  -> do
                          (w, c3) <- getU8 c2
                          go c3 (bump 3 used) (pushE (SV (lit "UInt8" (show w))) est)

                      | tag == tagW16 -> do
                          (w, c3) <- getU16BE c2
                          go c3 (bump 4 used) (pushE (SV (lit "UInt16" (show w))) est)

                      | tag == tagW32 -> do
                          (w, c3) <- getU32BE c2
                          go c3 (bump 6 used) (pushE (SV (lit "UInt32" (show w))) est)

                      | tag == tagW64 -> do
                          (w, c3) <- getU64BE c2
                          go c3 (bump 10 used) (pushE (SV (lit "UInt64" (show w))) est)

                      | tag == tagFlt -> do
                          (w, c3) <- getU32BE c2
                          go c3 (bump 6 used) (pushE (SV (lit "Float" (show (word32ToFloat w)))) est)

                      | tag == tagDbl -> do
                          (w, c3) <- getU64BE c2
                          go c3 (bump 10 used) (pushE (SV (lit "Double" (show (word64ToDouble w)))) est)

                      | otherwise ->
                          Left ("Unknown value tag: 0x" ++ hex2 tag)

            _ | op == opcPushFromArgs -> do
                  (ix, c2) <- getI32BE c1
                  let nm = "a" ++ show ix
                  go c2 (bump 5 used)
                     (pushE (SV (EVar nm)) est { maxArg = max (maxArg est) ix })

              | op == opcPushFromEnv -> do
                  ((nm, nB), c2) <- getString c1
                  go c2 (bump (1 + 4 + nB) used)
                     (pushE (SV (EVar nm)) est)

              | op == (opcCall) || op == (opcTailCall) -> do
                  (arity, c2) <- getI32BE c1
                  (args, est1) <- popN arity est
                  (fn, est2)   <- popE est1
                  let push v = go c2 (bump 5 used) (pushE (SV v) est2)
                  case fn of
                    SOP o -> push (EBin o args)
                    SV f  -> push (ECall f args)
                    _     -> Left "Call on raw func bytes"

              | op == opcJumpIfFalse -> do
                  (offB, c2) <- getI32BE c1
                  (cond, est1) <- popExpr est
                  -- offB includes: THEN bytes + (1 byte jump opcode + 4 bytes else size)
                  let thenSize = offB - 5
                  if thenSize < 0
                    then Left "Bad JumpIfFalse"
                    else do
                      (thenBs, afterThen) <- takeN (fromIntegral thenSize) c2
                      (thenE, _, _) <- deExpr (Cur thenBs (off c2)) (BL.length thenBs)

                      (jmp, cJ) <- getU8 afterThen
                      if jmp /= opcJump
                        then Left "Expected Jump"
                        else do
                          (elseSz, cE0) <- getI32BE cJ
                          (elseBs, rest) <- takeN (fromIntegral elseSz) cE0
                          (elseE, _, _) <- deExpr (Cur elseBs (off cE0)) (BL.length elseBs)

                          let e' = EIf cond thenE elseE
                          go (Cur (input rest) (off rest)) used
                             (pushE (SV e') est1)

              | op == opcJump -> do
                  (n, c2) <- getI32BE c1
                  (_, rest) <- takeN (fromIntegral n) c2
                  go (Cur (input rest) (off rest)) used est

              | op == opcPop -> do
                  _ <- popExpr est
                  go c1 used est

              | op == opcReturn ->
                  case stk est of
                    (SV v:_) -> Right (normalize v, max 0 (maxArg est + 1), c1)
                    _        -> Left "Return without value"

              | op == opcDefine -> do
                  ((_, nB), c2) <- getString c1
                  go c2 (bump (1 + 4 + nB) used) est

              | op == opcAssign -> do
                  ((_, nB), c2) <- getString c1
                  _ <- popExpr est
                  go c2 (bump (1 + 4 + nB) used) est

              | otherwise ->
                  Left ("Unknown opcode in expr: 0x" ++ hex2 op)

    popN :: Int -> Est -> Either String ([Expr], Est)
    popN 0 e = Right ([], e)
    popN n e = do
      (xs, e1) <- popN (n - 1) e
      (a, e2)  <- popExpr e1
      pure (a:xs, e2)

------------------------------------------------------------
-- Top-level decoder (rare but kept)
------------------------------------------------------------

data Tst = Tst { tstk :: [SI], tout :: [Top] } deriving (Show)
emptyT :: Tst; emptyT = Tst [] []

pushT :: SI -> Tst -> Tst
pushT x t = t { tstk = x : tstk t }

popT :: Tst -> Either String (SI, Tst)
popT t = case tstk t of
  []     -> Left "Stack underflow"
  (x:xs) -> Right (x, t { tstk = xs })

popExprT :: Tst -> Either String (Expr, Tst)
popExprT t = do
  (si, t') <- popT t
  case si of
    SV v -> Right (v, t')
    _    -> Left "Expected expr"

deTop :: Cur -> Int64 -> Either String ([Top], Cur)
deTop c0 lim = go c0 0 emptyT
  where
    finish :: Tst -> Either String ([Top], Cur)
    finish t = Right (reverse (tout t), c0 { input = BL.drop lim (input c0)
                                           , off   = off c0 + lim })

    bump :: Int64 -> Int64 -> Int64
    bump n m = m + n

    go :: Cur -> Int64 -> Tst -> Either String ([Top], Cur)
    go c used st
      | used >= lim || atEnd c = finish st
      | otherwise = do
          (op, c1) <- getU8 c
          case op of
            _ | op == opcPush -> do
                  (tag, c2) <- getU8 c1
                  case tag of
                    _ | tag == tagBool -> do
                          (b, c3) <- getU8 c2
                          go c3 (bump 3 used)
                             (pushT (SV (lit "Bool" (if b == 0 then "false" else "true"))) st)

                      | tag == tagOp -> do
                          (b, c3) <- getU8 c2
                          o <- opFromByte b
                          go c3 (bump 3 used) (pushT (SOP o) st)

                      | tag == tagFunc -> do
                          (bs, c3) <- readFuncValBody c2
                          go c3 (bump (1 + 4 + BL.length bs) used) (pushT (SF bs) st)

                      | tag == tagI8 -> do
                          (w, c3) <- getU8 c2
                          let i = fromIntegral (fromIntegral w :: Int8)
                              s = if i >= 32 && i <= 126 && isPrint (chr i)
                                    then ['\'', chr i, '\'']
                                    else show i
                          go c3 (bump 3 used) (pushT (SV (lit "Int8" s)) st)

                      | tag == tagI16 -> do
                          (i, c3) <- getI16BE c2
                          go c3 (bump 4 used) (pushT (SV (lit "Int16" (show i))) st)

                      | tag == tagI32 -> do
                          (i, c3) <- getI32BE c2
                          go c3 (bump 6 used) (pushT (SV (lit "Int32" (show i))) st)

                      | tag == tagI64 -> do
                          (i, c3) <- getI64BE c2
                          go c3 (bump 10 used) (pushT (SV (lit "Int64" (show i))) st)

                      | tag == tagW8  -> do
                          (w, c3) <- getU8 c2
                          go c3 (bump 3 used) (pushT (SV (lit "UInt8" (show w))) st)

                      | tag == tagW16 -> do
                          (w, c3) <- getU16BE c2
                          go c3 (bump 4 used) (pushT (SV (lit "UInt16" (show w))) st)

                      | tag == tagW32 -> do
                          (w, c3) <- getU32BE c2
                          go c3 (bump 6 used) (pushT (SV (lit "UInt32" (show w))) st)

                      | tag == tagW64 -> do
                          (w, c3) <- getU64BE c2
                          go c3 (bump 10 used) (pushT (SV (lit "UInt64" (show w))) st)

                      | tag == tagFlt -> do
                          (w, c3) <- getU32BE c2
                          go c3 (bump 6 used) (pushT (SV (lit "Float" (show (word32ToFloat w)))) st)

                      | tag == tagDbl -> do
                          (w, c3) <- getU64BE c2
                          go c3 (bump 10 used) (pushT (SV (lit "Double" (show (word64ToDouble w)))) st)

                      | otherwise ->
                          Left ("Unknown value tag: 0x" ++ hex2 tag)

            _ | op == opcPushFromArgs -> do
                  (ix, c2) <- getI32BE c1
                  go c2 (bump 5 used)
                     (pushT (SV (EVar ("a" ++ show ix))) st)

              | op == opcPushFromEnv -> do
                  ((nm, nB), c2) <- getString c1
                  go c2 (bump (1 + 4 + nB) used)
                     (pushT (SV (EVar nm)) st)

              | op == opcCall || op == opcTailCall -> do
                  (arity, c2) <- getI32BE c1
                  (args, st1) <- popNT arity st
                  (fn,   st2) <- popT st1
                  let push v = go c2 (bump 5 used) (pushT (SV v) st2)
                  case fn of
                    SOP o -> push (EBin o args)
                    SV f  -> push (ECall f args)
                    _     -> Left "Call on raw func bytes"

              | op == opcDefine -> do
                  ((nm, nB), c2) <- getString c1
                  case tstk st of
                    (SF bs : rest) -> do
                      let cur = Cur bs 0
                          len = BL.length bs
                      (body, argc, _) <- deExpr cur len
                      -- pretty arg names
                      let rawParams    = [ "a" ++ show i | i <- [0 .. argc - 1] ]
                          prettyParams =
                            case rawParams of
                              ["a0"] -> ["n"]
                              xs     -> xs
                          mapping = zip rawParams prettyParams
                          body'   = substExpr mapping body
                      go c2 (bump (1 + 4 + nB) used)
                        (emit (TFunc nm prettyParams body') st { tstk = rest })
                    _ -> do
                      (rhs, st1) <- popExprT st
                      go c2 (bump (1 + 4 + nB) used)
                        (emit (TVar (inferTypeShallow rhs) nm rhs) st1)

              | op == opcAssign -> do
                  ((nm, nB), c2) <- getString c1
                  (rhs, st1)     <- popExprT st
                  let asnTop = TExpr (ECall (EVar ":=") [EVar nm, rhs])
                  go c2 (bump (1 + 4 + nB) used)
                     (emit asnTop st1)

              | op == opcJumpIfFalse -> do
                  (offB, c2)  <- getI32BE c1
                  (cond, st1) <- popExprT st
                  let thenSize = offB - 5
                  if thenSize < 0
                    then Left "Bad JumpIfFalse"
                    else do
                      (thenBs, afterThen) <- takeN (fromIntegral thenSize) c2
                      (tE, _, _)          <- deExpr (Cur thenBs (off c2)) (BL.length thenBs)

                      (jmp, cJ) <- getU8 afterThen
                      if jmp /= opcJump
                        then Left "Expected Jump"
                        else do
                          (elseSz, cE0) <- getI32BE cJ
                          (elseBs, rest) <- takeN (fromIntegral elseSz) cE0
                          (eE, _, _) <- deExpr (Cur elseBs (off cE0)) (BL.length elseBs)

                          let st' = pushT (SV (EIf cond tE eE)) st1
                          go (Cur (input rest) (off rest)) used st'

              | op == opcPop -> do
                  (e, st1) <- popExprT st
                  go c1 used (emit (TExpr e) st1)

              | op == opcReturn ->
                  finish st

              | op == opcJump -> do
                  (n, c2)   <- getI32BE c1
                  (_, rest) <- takeN (fromIntegral n) c2
                  go (Cur (input rest) (off rest)) used st

              | otherwise ->
                  Left ("Unknown opcode: 0x" ++ hex2 op)

    emit :: Top -> Tst -> Tst
    emit a t = t { tout = a : tout t }

    popNT :: Int -> Tst -> Either String ([Expr], Tst)
    popNT 0 t = Right ([], t)
    popNT n t = do
      (xs, t1) <- popNT (n - 1) t
      (a,  t2) <- popExprT t1
      pure (a : xs, t2)

------------------------------------------------------------
-- Rendering with typed params / returns
------------------------------------------------------------

renderProgram :: [Top] -> String
renderProgram tops =
  -- order: all funcs first, then vars, then exprs
  let fs = [ t | t@TFunc{} <- tops ]
      vs = [ t | t@TVar{}  <- tops ]
      es = [ t | t@TExpr{} <- tops ]
  in (++ "\n") . L.intercalate "\n\n" $ map renderTop (fs ++ vs ++ es)

renderTop :: Top -> String
renderTop (TVar (Just ty) n e) =
  "define " ++ ty ++ " " ++ n ++ " = " ++ render e
renderTop (TVar Nothing  n e)  =
  "define " ++ n  ++ " = " ++ render e

renderTop (TFunc nm ps body) =
  let retTy   = inferExprTypeDeep body           -- e.g. Just "Int32"
      showTy  = maybe "Any" id retTy             -- default Any
      psTyped = case retTy of
                  Just t  -> L.intercalate ", " [ t ++ " " ++ p | p <- ps ]
                  Nothing -> L.intercalate ", " ps
      header =
        "func " ++ nm ++ "<" ++ psTyped ++ "> => " ++ showTy
      bodyBlk = renderGuards nm body
  in header ++ "\n\n" ++ bodyBlk

renderTop (TExpr e) =
  render e

renderGuards :: String -> Expr -> String
renderGuards n e =
  "$ " ++ n ++ "\n" ++ unlines (map clause (guards e))
  where
    clause (Just c, t)  = "    " ++ render c ++ " -> " ++ render t
    clause (Nothing, t) = "    -> " ++ render t

    guards :: Expr -> [(Maybe Expr, Expr)]
    guards (EIf c t f)  = (Just c, t) : guards f
    guards x            = [(Nothing, x)]

------------------------------------------------------------
-- Expr pretty-printer
------------------------------------------------------------

render :: Expr -> String
render = go 0 . normalize
  where
    go _ (ELit (Lit _ s)) = s
    go _ (EVar s)         = s
    go _ (EBin _ [])      = "<?>"

    -- unary
    go p (EBin o [a]) =
      par (p > 7) $ uSym o ++ atom a

    -- binary
    go p (EBin o [a,b]) =
      par (p > prec o) $ go (prec o) a ++ " " ++ bSym o ++ " " ++ go (prec o) b

    -- n-ary (shouldn't really happen in your code except maybe chains)
    go p (EBin o xs@(_:_ : _ : _)) =
      let k = prec o
          pieces = map (go (k + 1)) xs
      in par (p > k) (L.intercalate (" " ++ bSym o ++ " ") pieces)

    -- calls
    go _ (ECall f []) = go 9 f
    go p (ECall f as) =
      par (p > 8) $ L.intercalate " " (go 8 f : map arg as)

    -- inline if
    go p (EIf c t e) =
      par (p > 0) $
        "if " ++ go 0 c ++ " then " ++ go 0 t ++ " else " ++ go 0 e

    arg, atom :: Expr -> String
    arg x = case x of
      EVar _ -> go 9 x
      ELit _ -> go 9 x
      _      -> "(" ++ go 0 x ++ ")"
    atom = arg

    par :: Bool -> String -> String
    par True  s = "(" ++ s ++ ")"
    par False s = s

    -- operator precedence (bigger = tighter)
    prec :: Op -> Int
    prec o = case o of
      Or  -> 1
      And -> 2
      Xor -> 2
      Eq  -> 3
      NE  -> 3
      Lt  -> 4
      LE  -> 4
      Gt  -> 4
      GE  -> 4
      Add -> 5
      Sub -> 5
      Mul -> 6
      Div -> 6
      Mod -> 6
      _   -> 6

    uSym :: Op -> String
    uSym Neg = "-"
    uSym Not = "!"
    uSym _   = "<?>"

    bSym :: Op -> String
    bSym Add = "+"
    bSym Sub = "-"
    bSym Mul = "*"
    bSym Div = "/"
    bSym Mod = "%"
    bSym Eq  = "=="
    bSym NE  = "!="
    bSym Lt  = "<"
    bSym LE  = "<="
    bSym Gt  = ">"
    bSym GE  = ">="
    bSym And = "&&"
    bSym Or  = "||"
    bSym Xor = "xor"
    bSym _   = "<?>"

------------------------------------------------------------
-- Public API
------------------------------------------------------------

decompileToString :: BL.ByteString -> Either String String
decompileToString bs = do
  let c0 = Cur bs 0
  (m, c1) <- getU32BE c0
  if m /= magicNumber then Left "Bad magic" else do
    (v, c2) <- getU32BE c1
    if v /= currentVersion
      then Left ("Unsupported version: " ++ show v)
      else do
        (envTops, c3) <- parseEnv c2
        let code = input c3
            len  = BL.length code
        (tops, _) <- deTop (Cur code (off c3)) len
        pure (renderProgram (envTops ++ tops))

writeDecompiled :: FilePath -> FilePath -> IO (Either String ())
writeDecompiled inp out = do
  bs <- BL.readFile inp
  case decompileToString bs of
    Left e  -> pure (Left e)
    Right s -> do
      writeFile out s
      pure (Right ())
