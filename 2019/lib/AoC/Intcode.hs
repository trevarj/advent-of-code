module AoC.Intcode (interpret, Program) where

import Data.Maybe (fromMaybe)
import Data.Vector ((//))
import qualified Data.Vector as V

type Program = V.Vector Int

data OpCode = Add Int Int Int | Mul Int Int Int | Halt

data Ret = Put Int Int | Exit

doOp :: OpCode -> Ret
doOp (Add a b dest) = Put (a + b) dest
doOp (Mul a b dest) = Put (a * b) dest
doOp Halt = Exit

interpret :: Program -> Int {- IP -} -> Program
interpret prog ip = handleRet . doOp . parseOp $ val ip
  where
    parseOp :: Int -> OpCode
    parseOp 1 = Add (drefOff 1) (drefOff 2) (valOff 3)
    parseOp 2 = Mul (drefOff 1) (drefOff 2) (valOff 3)
    parseOp 99 = Halt
    parseOp _ = error "invalid opcode"

    handleRet :: Ret -> Program
    handleRet (Put v addr) = interpret (prog // [(addr, v)]) (ip + 4)
    handleRet Exit = prog

    -- get value at address found at index
    dref :: Int {- index -} -> Int
    dref = val . val

    -- get value at address found at IP + offse
    drefOff :: Int {- offset -} -> Int
    drefOff off = dref (ip + off)

    -- get value at program index
    val :: Int {- index -} -> Int
    val ix = fromMaybe 0 $ (V.!?) prog ix

    -- get value at IP + offset
    valOff :: Int {- offset -} -> Int
    valOff off = val (ip + off)
