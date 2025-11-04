{-# LANGUAGE Arrows #-} 
module Cpu where

import FRP.Yampa
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Signals

type Address = Int
type Memory = Vector Int
type Data = Int

defaultData :: Data
defaultData = 0

data Instruction = LoadA Address | StoreA Address | Add Address | NoOp | Out | Halt | Jump Address
  deriving (Show, Eq)

data ControlSignal
  = ARegisterIn
  | ARegisterOut
  | BRegisterIn
  | ALUOut
  | OutRegisterIn
  | CounterOut
  | CounterEnable
  | MemoryAddressIn
  | RAMIn
  | RAMOut
  | InstructionRegisterIn
  | InstructionRegisterOut -- only lower 4 bits go to bus
  | JumpFlag
  deriving (Show, Eq)

type MicroInstruction = [ControlSignal]

fetchInstruction :: Vector MicroInstruction
fetchInstruction = V.fromList [
  [CounterOut, MemoryAddressIn],
  [RAMOut, InstructionRegisterIn, CounterEnable]
  ]

-- does not include fetchInstruction
loadA :: Vector MicroInstruction
loadA = V.fromList [
  [InstructionRegisterOut, MemoryAddressIn],
  [RAMOut, ARegisterIn]
  ]

storeA :: Vector MicroInstruction
storeA = V.fromList [
  [InstructionRegisterOut, MemoryAddressIn],
  [ARegisterOut, RAMIn]
  ]

addI :: Vector MicroInstruction
addI = V.fromList [
  [InstructionRegisterOut, MemoryAddressIn],
  [RAMOut, BRegisterIn],
  [ALUOut, ARegisterIn]
  ]

type Register = SF (Bool, Data) Data

aRegister :: Register
aRegister = latch 0


counterRegister :: SF (Bool, Bool, Bool, Int) Int
counterRegister = let
    adapter (cl, inc, w, d) = (cl, (inc, w, d)) -- adapt to fLatch input
    update_ i (inc, w, d) = case (inc, w) of
      (True, False) -> i + 1
      (_, True) -> d
      _ -> i
  in
    arr adapter >>> fLatch update_ defaultData


maxMicroCycles :: Int
maxMicroCycles = 5
-- input to microinstruction counter is inverted
microCounter :: SF Bool Int
microCounter = arr not >>> cycleCounter (maxMicroCycles + 1)

-- True if writing
ram :: Memory -> SF (Bool, Address, Data) Data
ram initContents = proc (write, addr, dat) -> do
  memory <- fLatch f initContents -< (write, (addr, dat))
  returnA -< memory V.! addr
  where
    f m (a, d) = m V.// [(a, d)]

getInstructionAddress :: Data -> Address
getInstructionAddress d = d `mod` 16 -- lower 4 bits

decodeInstruction :: Data -> Instruction
decodeInstruction n = let
  lower = n `mod` 16
  upper = n `div` 16
  in case upper of
    1 -> LoadA lower
    2 -> Add lower
    3 -> Out
    4 -> StoreA lower
    0 -> NoOp
    5 -> Halt
    6 -> Jump lower
    _ -> Halt

encodeInstruction :: Instruction -> Data
encodeInstruction i = let
  combine upper lower = upper * 16 + lower
  in case i of
    LoadA addr -> combine 1 addr
    Add addr -> combine 2 addr
    Out -> combine 3 0
    StoreA addr -> combine 4 addr
    NoOp -> combine 0 0
    Halt -> combine 5 0
    Jump addr -> combine 6 addr

getMicroInstructions :: Instruction -> Vector MicroInstruction
getMicroInstructions i = case i of
  Halt -> mempty
  _ -> fetchInstruction <> case i of
    LoadA _ -> loadA
    StoreA _ -> storeA
    Add _ -> addI
    NoOp -> mempty
    Out -> V.fromList [[ARegisterOut, OutRegisterIn]]
    Jump _ -> V.fromList [[InstructionRegisterOut, JumpFlag]]

instructions :: [Instruction]
instructions = [LoadA 5, Add 5, Out, StoreA 5, Jump 0]

initialMemory :: Memory
initialMemory = V.fromList $ (encodeInstruction <$> instructions) <> [1]


data CPUState = CPUState {
  cpuA :: Data,
  cpuOut :: Data,
  cpuCounter :: Data,
  cpuBus :: Data,
  cpuMicro :: MicroInstruction,
  cpuInstruction :: Data
}
  deriving Show

cpuSignal :: SF a CPUState
cpuSignal = proc _ -> do
  c <- clock 1.0 -< ()
  microN <- microCounter -< c
  rec
    let instructionList' = getMicroInstructions (decodeInstruction instruction)
    instructionList <- iPre (getMicroInstructions NoOp) -< instructionList' --provide base case
    let microInst = fromMaybe [] $ instructionList V.!? microN
    let enabled = (`elem` microInst)
    let addrIn = enabled MemoryAddressIn
    let iIn = enabled InstructionRegisterIn
    let ramIn = enabled RAMIn
    let aIn = enabled ARegisterIn
    let bIn = enabled BRegisterIn
    let outIn = enabled OutRegisterIn
    let bus
          | enabled CounterOut = n
          | enabled RAMOut = m
          | enabled InstructionRegisterOut = getInstructionAddress instruction
          | enabled ARegisterOut = a
          | enabled ALUOut = s
          | otherwise = 0
    n <- counterRegister -< (c, enabled CounterEnable, enabled JumpFlag, bus) -- counter
    instruction <- aRegister -< (iIn && c, bus) -- instruction register
    addr <- aRegister -< (addrIn && c, bus) -- address register
    m <- ram initialMemory -< (ramIn && c, addr, bus) -- ram
    a <- aRegister -< (aIn && c, bus) -- A register
    b <- aRegister -< (bIn && c, bus) -- B register
    s <- iPre 0 -< a + b -- need to introduce delay on sum
    out <- aRegister -< (outIn && c, bus) -- out register

  returnA -< CPUState {cpuA=a, cpuOut=out, cpuCounter=n, cpuBus = bus, cpuMicro=microInst, cpuInstruction=instruction}