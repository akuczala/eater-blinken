module Instructions
  ( ControlSignal (..),
    Instruction (..),
    MicroInstruction,
    Flag (..),
    encodeFlags,
    decodeFlags,
    encodeInstruction,
    decodeInstruction,
    getInstructionAddress,
    getMicroInstructions,
  )
where

import Data.Bits (Bits (bit), testBit)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Signals

data ControlSignal
  = ARegisterIn
  | ARegisterOut
  | BRegisterIn
  | ALUOut
  | Subtract
  | OutRegisterIn
  | CounterOut
  | CounterEnable
  | MemoryAddressIn
  | RAMIn
  | RAMOut
  | InstructionRegisterIn
  | InstructionRegisterOut
  | JumpSignal
  | FlagRegisterIn
  deriving (Show, Eq)

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
  | LoadI Address
  | Sub Address
  deriving (Show, Eq)

data InstructionTag
  = LoadATag
  | StoreATag
  | AddTag
  | NoOpTag
  | OutTag
  | HaltTag
  | JumpTag
  | JEZTag
  | JLZTag
  | LoadITag
  | SubTag
  deriving (Show, Eq, Enum)

tagToInstruction :: InstructionTag -> Address -> Instruction
tagToInstruction t d = case t of
  LoadATag -> LoadA d
  StoreATag -> StoreA d
  AddTag -> Add d
  NoOpTag -> NoOp
  OutTag -> Out
  HaltTag -> Halt
  JumpTag -> Jump d
  JEZTag -> JEZ d
  JLZTag -> JLZ d
  LoadITag -> LoadI d
  SubTag -> Sub d

instructionToTag :: Instruction -> (InstructionTag, Address)
instructionToTag i = case i of
  LoadA a -> (LoadATag, a)
  StoreA a -> (StoreATag, a)
  Add a -> (AddTag, a)
  NoOp -> (NoOpTag, defaultData)
  Out -> (OutTag, defaultData)
  Halt -> (HaltTag, defaultData)
  Jump a -> (JumpTag, a)
  JEZ a -> (JEZTag, a)
  JLZ a -> (JLZTag, a)
  LoadI a -> (LoadITag, a)
  Sub a -> (SubTag, a)

getInstructionAddress :: Data -> Address
getInstructionAddress d = d `mod` 16 -- lower 4 bits

decodeInstruction :: Data -> Instruction
decodeInstruction n =
  let lower = n `mod` 16
      upper = n `div` 16
   in tagToInstruction (toEnum $ fromIntegral upper) lower

encodeInstruction :: Instruction -> Data
encodeInstruction i =
  let (t, a) = instructionToTag i
      combine upper lower = upper * 16 + (lower `mod` 16)
   in combine (fromIntegral $ fromEnum t) a

type MicroInstruction = [ControlSignal]

data Flag = ZeroFlag | NegativeFlag
  deriving (Show, Eq, Enum)

possibleFlags :: [Flag]
possibleFlags = [ZeroFlag, NegativeFlag]

decodeFlags :: Data -> [Flag]
decodeFlags d = mapMaybe decodeFlag possibleFlags
  where
    decodeFlag flag = if d `testBit` fromEnum flag then Just flag else Nothing

encodeFlags :: [Flag] -> Data
encodeFlags flags = sum $ fmap encodeFlag possibleFlags
  where
    encodeFlag flag = if flag `elem` flags then bit (fromEnum flag) else 0

fetchInstruction :: Vector MicroInstruction
fetchInstruction =
  V.fromList
    [ [CounterOut, MemoryAddressIn],
      [RAMOut, InstructionRegisterIn, CounterEnable]
    ]

-- does not include fetchInstruction
loadA :: Vector MicroInstruction
loadA =
  V.fromList
    [ [InstructionRegisterOut, MemoryAddressIn],
      [RAMOut, ARegisterIn]
    ]

storeA :: Vector MicroInstruction
storeA =
  V.fromList
    [ [InstructionRegisterOut, MemoryAddressIn],
      [ARegisterOut, RAMIn]
    ]

addI :: Vector MicroInstruction
addI =
  V.fromList
    [ [InstructionRegisterOut, MemoryAddressIn],
      [RAMOut, BRegisterIn],
      [ALUOut, ARegisterIn, FlagRegisterIn]
    ]

subI :: Vector MicroInstruction
subI =
  V.fromList
    [ [InstructionRegisterOut, MemoryAddressIn],
      [RAMOut, BRegisterIn],
      [ALUOut, ARegisterIn, FlagRegisterIn, Subtract]
    ]

getMicroInstructions :: [Flag] -> Instruction -> Vector MicroInstruction
getMicroInstructions flags i = case i of
  Halt -> mempty
  _ ->
    fetchInstruction <> case i of
      LoadA _ -> loadA
      StoreA _ -> storeA
      Add _ -> addI
      NoOp -> mempty
      Out -> V.fromList [[ARegisterOut, OutRegisterIn]]
      Jump _ -> jumpInstructions True
      JEZ _ -> jumpInstructions (ZeroFlag `elem` flags)
      JLZ _ -> jumpInstructions (NegativeFlag `elem` flags)
      LoadI _ ->
        V.fromList
          [ [InstructionRegisterOut, ARegisterIn]
          ]
      Sub _ -> subI
    where
      jumpInstructions doJump = V.fromList [[InstructionRegisterOut] <> ([JumpSignal | doJump])]
