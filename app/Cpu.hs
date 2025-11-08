{-# LANGUAGE Arrows #-}

module Cpu
  ( CPUState (..),
    cpuSignal,
    encodeProgram,
  )
where

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Vector as V
import FRP.Yampa
import Instructions
import Signals

type Register = SF (Bool, Data) Data

aRegister :: Register
aRegister = latch defaultData

counterRegister :: (Num a) => a -> SF (Bool, Bool, Bool, a) a
counterRegister defaultVal =
  let adapter (cl, inc, w, d) = (cl, (inc, w, d)) -- adapt to fLatch input
      update_ i (inc, w, d) = case (inc, w) of
        (True, False) -> i + 1
        (_, True) -> d
        _ -> i
   in arr adapter >>> fLatch update_ defaultVal

maxMicroCycles :: Data
maxMicroCycles = 4

-- input to microinstruction counter is inverted
microCounter :: SF Bool Data
microCounter = arr not >>> cycleCounter (maxMicroCycles + 1)

-- True if writing
ram :: Memory -> SF (Bool, Address, Data) Data
ram initContents = proc (write, addr, dat) -> do
  let iAddr = fromIntegral addr
  memory <- fLatch f initContents -< (write, (iAddr, dat))
  returnA -< memory V.! iAddr
  where
    f m (a, d) = m V.// [(a, d)]

encodeProgram :: (Address -> Address -> [Instruction]) -> Memory
encodeProgram program =
  let programLen = fromIntegral . length $ program 0 0
      prog = program programLen (programLen + 1)
   in V.fromList $ (encodeInstruction <$> prog) <> [0, 0]

data CPUState = CPUState
  { cpuA :: Data,
    cpuB :: Data,
    cpuAlu :: Data,
    cpuOut :: Data,
    cpuCounter :: Data,
    cpuBus :: Data,
    cpuMicro :: MicroInstruction,
    cpuMicroCounter :: Data,
    cpuInstruction :: Data,
    cpuAddr :: Data,
    cpuMemory :: Data,
    cpuFlags :: [Flag]
  }
  deriving (Show)

aluSignal :: SF (Data, Data, Bool) (Data, Bool, Bool)
aluSignal = proc (a, b, sub) -> do
  let b' = fromIntegral b :: Int
  let b'' = if sub then -b' else b'
  let s = fromIntegral a + fromIntegral b'' :: Int
  returnA -< (fromIntegral s, s == 0, s < 0)

-- get flags from aluOut
getFlags :: Bool -> Bool -> [Flag]
getFlags zeroFlagOn negativeFlagOn = catMaybes [f ZeroFlag zeroFlagOn, f NegativeFlag negativeFlagOn]
  where
    f flag enable = if enable then Just flag else Nothing

cpuSignal :: Memory -> SF Bool CPUState
cpuSignal initialMemory = proc c -> do
  microN <- microCounter -< c
  rec let instructionList' = getMicroInstructions flags (decodeInstruction instruction)
      instructionList <- iPre (getMicroInstructions [] NoOp) -< instructionList' -- provide base case
      let microInst = fromMaybe [] $ instructionList V.!? fromIntegral microN
      let enabled = (`elem` microInst)
      let bus
            | enabled CounterOut = n
            | enabled RAMOut = m
            | enabled InstructionRegisterOut = getInstructionAddress instruction -- only address goes to the bus
            | enabled ARegisterOut = a
            | enabled ALUOut = s
            | otherwise = 0
      n <- counterRegister defaultData -< (c, enabled CounterEnable, enabled JumpSignal, bus) -- counter
      instruction <- aRegister -< (enabled InstructionRegisterIn && c, bus) -- instruction register
      addr <- aRegister -< (enabled MemoryAddressIn && c, bus) -- address register
      m <- ram initialMemory -< (enabled RAMIn && c, addr, bus) -- ram
      a <- aRegister -< (enabled ARegisterIn && c, bus) -- A register
      b <- aRegister -< (enabled BRegisterIn && c, bus) -- B register
      out <- aRegister -< (enabled OutRegisterIn && c, bus) -- out register
      (s, isZero, isNeg) <- iPre (defaultData, False, False) <<< aluSignal -< (a, b, enabled Subtract) -- need to introduce delay on sum
      flags <- arr decodeFlags <<< aRegister -< (enabled FlagRegisterIn && c, encodeFlags $ getFlags isZero isNeg)

  returnA
    -<
      CPUState
        { cpuA = a,
          cpuB = b,
          cpuAlu = s,
          cpuOut = out,
          cpuCounter = n,
          cpuMicroCounter = microN,
          cpuBus = bus,
          cpuMicro = microInst,
          cpuInstruction = instruction,
          cpuAddr = addr,
          cpuMemory = m,
          cpuFlags = flags
        }
