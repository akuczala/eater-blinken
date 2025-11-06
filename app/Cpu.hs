{-# LANGUAGE Arrows #-}
module Cpu where

import FRP.Yampa
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Signals
import Data.Maybe (catMaybes)
import Data.Bits (bit, Bits (testBit))

type Address = Int
type Memory = Vector Int
type Data = Int

defaultData :: Data
defaultData = 0

data Instruction
  = LoadA Address
  | StoreA Address
  | Add Address
  | NoOp
  | Out
  | Halt
  | Jump Address
  | JEZ Address
  | JLZ Address
  | LoadI Data
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
  | JumpSignal
  | FlagRegisterIn
  deriving (Show, Eq)

type MicroInstruction = [ControlSignal]

data Flag = ZeroFlag | NegativeFlag
  deriving (Show, Eq)

possibleFlags :: [Flag]
possibleFlags = [ZeroFlag, NegativeFlag]

decodeFlags :: Data -> [Flag]
--decodeFlags d = catMaybes [if d `testBit` 1 then Just ZeroFlag else Nothing]
decodeFlags d = catMaybes $ zipWith decodeFlag [0..] possibleFlags where
  decodeFlag i flag = if d `testBit` i then Just flag else Nothing

encodeFlags :: [Flag] -> Data
encodeFlags flags = sum $ zipWith encodeFlag [0..] possibleFlags  where
  encodeFlag i flag = if flag `elem` flags then bit i else 0

-- get flags from aluOut
getFlags :: Data -> [Flag]
getFlags aluOut = catMaybes $ zipWith f flagConditions possibleFlags where
  f enable flag = if enable then Just flag else Nothing
  flagConditions = [aluOut == 0, aluOut < 0]

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
  [ALUOut, ARegisterIn, FlagRegisterIn]
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
maxMicroCycles = 4
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
    7 -> JEZ lower
    8 -> JLZ lower
    9 -> LoadI lower
    _ -> Halt

encodeInstruction :: Instruction -> Data
encodeInstruction i = let
  combine upper lower = upper * 16 + (lower `mod` 16)
  in case i of
    LoadA addr -> combine 1 addr
    Add addr -> combine 2 addr
    Out -> combine 3 0
    StoreA addr -> combine 4 addr
    NoOp -> combine 0 0
    Halt -> combine 5 0
    Jump addr -> combine 6 addr
    JEZ addr -> combine 7 addr
    JLZ addr -> combine 8 addr
    LoadI dat -> combine 9 dat

getMicroInstructions :: [Flag] -> Instruction -> Vector MicroInstruction
getMicroInstructions flags i = case i of
  Halt -> mempty
  _ -> fetchInstruction <> case i of
    LoadA _ -> loadA
    StoreA _ -> storeA
    Add _ -> addI
    NoOp -> mempty
    Out -> V.fromList [[ARegisterOut, OutRegisterIn]]
    Jump _ -> jumpInstructions True
    JEZ _ -> jumpInstructions (ZeroFlag `elem` flags)
    JLZ _ -> jumpInstructions (NegativeFlag `elem` flags)
    LoadI _ -> V.fromList [
      [InstructionRegisterOut, ARegisterIn]
      ]
    where
      jumpInstructions doJump = V.fromList [[InstructionRegisterOut] <> ([JumpSignal | doJump])]

doublingProgram :: Int -> [Instruction]
doublingProgram x = [LoadI 1, StoreA x, LoadA x, Add x, Out, StoreA x, Jump 0]

fibProgram :: Int -> Int -> [Instruction]
fibProgram x y = [
  LoadI 1, StoreA x, StoreA y,
  LoadA x, Add y, Out, StoreA x,
  Add y, Out, StoreA y,
  Jump 3
  ]

countDown :: Int -> Int -> [Instruction]
countDown x y = [
  LoadI (-1),
  StoreA x,
  LoadI 3,
  StoreA y,
  Add x, -- 4
  Out,
  JEZ 8,
  Jump 4,
  LoadI 1, -- 8
  Add y,
  Jump 4
  ]

countDownStop :: Int -> Int -> [Instruction]
countDownStop x y = [
  LoadI (-1),
  StoreA x,
  LoadI 5,
  StoreA y,
  Add x, -- 4
  Out,
  JEZ 8,
  Jump 4,
  Halt -- 8
  ]

initialMemory :: Memory
initialMemory = let
  programLen = length $ countDownStop 0 0
  program = countDownStop programLen (programLen + 1)
  in V.fromList $ (encodeInstruction <$> program) <> [0, 0]


data CPUState = CPUState {
  cpuA :: Data,
  cpuB :: Data,
  cpuAlu :: Data,
  cpuOut :: Data,
  cpuCounter :: Int,
  cpuBus :: Data,
  cpuMicro :: MicroInstruction,
  cpuMicroCounter :: Int,
  cpuInstruction :: Data,
  cpuAddr :: Data,
  cpuMemory :: Data,
  cpuFlags :: [Flag]
}
  deriving Show

cpuSignal :: SF Bool CPUState
cpuSignal = proc c -> do
  microN <- microCounter -< c
  rec
    let instructionList' = getMicroInstructions flags (decodeInstruction instruction)
    instructionList <- iPre (getMicroInstructions [] NoOp) -< instructionList' --provide base case
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
    n <- counterRegister -< (c, enabled CounterEnable, enabled JumpSignal, bus) -- counter
    instruction <- aRegister -< (iIn && c, bus) -- instruction register
    addr <- aRegister -< (addrIn && c, bus) -- address register
    m <- ram initialMemory -< (ramIn && c, addr, bus) -- ram
    a <- aRegister -< (aIn && c, bus) -- A register
    b <- aRegister -< (bIn && c, bus) -- B register
    s <- iPre 0 -< a + b -- need to introduce delay on sum
    flags <- arr decodeFlags <<< aRegister -< (enabled FlagRegisterIn && c, encodeFlags $ getFlags s)
    out <- aRegister -< (outIn && c, bus) -- out register

  returnA -< CPUState {
    cpuA=a,
    cpuB=b,
    cpuAlu=s,
    cpuOut=out,
    cpuCounter=n,
    cpuMicroCounter=microN,
    cpuBus = bus, cpuMicro=microInst,
    cpuInstruction=instruction,
    cpuAddr=addr,
    cpuMemory = m,
    cpuFlags=flags
    }