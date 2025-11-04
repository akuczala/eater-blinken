{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Main where
import FRP.Yampa
import Text.Printf (printf)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

type Address = Int
type Memory = Vector Int
type Data = Int

defaultData :: Data
defaultData = 0

data Instruction = LoadA Address | StoreA Address | Add Address | NoOp | Out | Halt | Jump Address
  deriving (Show, Eq)

data MicroInstruction
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

fetchInstruction :: Vector [MicroInstruction]
fetchInstruction = V.fromList [
  [CounterOut, MemoryAddressIn],
  [RAMOut, InstructionRegisterIn, CounterEnable]
  ]

-- does not include fetchInstruction
loadA :: Vector [MicroInstruction]
loadA = V.fromList [
  [InstructionRegisterOut, MemoryAddressIn],
  [RAMOut, ARegisterIn]
  ]

storeA :: Vector [MicroInstruction]
storeA = V.fromList [
  [InstructionRegisterOut, MemoryAddressIn],
  [ARegisterOut, RAMIn]
  ]

addI :: Vector [MicroInstruction]
addI = V.fromList [
  [InstructionRegisterOut, MemoryAddressIn],
  [RAMOut, BRegisterIn],
  [ALUOut, ARegisterIn]
  ]

outI :: Vector [MicroInstruction]
outI = V.fromList [[ARegisterOut, OutRegisterIn]]

type Register = SF (Bool, Data) Data
aRegister :: Register
aRegister = latch 0

inputInit :: Applicative f => f ()
inputInit = pure ()

input :: Applicative f => p -> f (Double, Maybe ())
input _ = pure (1.0, Just ())
output :: Show a => p -> a -> IO Bool
output _ o = print o >> pure False

run :: IO ()
run = do
  reactimate inputInit input output time

clock :: Time -> SF a Bool
clock t = accumHoldBy s False <<< repeatedly t () where
  s x _ = not x

counter :: SF Bool Int
counter = edge >>> accumHoldBy (\b _ -> b + 1) 0

cycleCounter :: Int -> SF Bool Int
cycleCounter resetAt = edge >>> accumHoldBy (\b _ -> (b + 1) `mod` resetAt) 0

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

fLatch :: (b -> a -> b) -> b -> SF (Bool, a) b
fLatch f init_ = stuff >>> accumHoldBy f init_ where
  stuff = proc (c, a) -> do
    e <- edge -< c
    returnA -< fmap (const a) e

latch :: a -> SF (Bool, a) a
latch init_ = stuff >>> accumHoldBy (\_ a -> a) init_ where
  stuff = proc (c, a) -> do
    e <- edge -< c
    returnA -< fmap (const a) e

unEdge :: SF (Event ()) Bool
unEdge = arr (fmap (const not)) >>> accumHold False

-- signal :: SF a Int
-- signal = proc x -> do
--   c <- clock 1.0 -< x
--   n <- counter -< c
--   ready <- hold False <<< after 4.4 True -< ()
--   out <- latch 0 -< (ready, n)
--   returnA -< out

step :: Time -> SF a Bool
step t1 = unEdge <<< after t1 ()

pulse :: Time -> Time -> SF a Bool
pulse t1 t2 = proc x -> do
  b1 <- unEdge <<< after t1 () -< x
  b2 <- unEdge <<< after t2 () -< x
  returnA -< b1 && not b2

-- testMemory :: SF a (Bool, Bool, Bool, Data)
-- testMemory = proc _ -> do
--   w <- (||) <$> pulse 1.0 2.0 <*> pulse 4.0 4.5-< ()
--   r <- (||) <$> pulse 2.5 3.0 <*> pulse 3.5 5.0 -< ()
--   wVal <- step 3.0 -< ()

--   d <- ram (V.fromList [1, 2]) -< (w, if r then 1 else 0, if wVal then 4 else 5)
--   returnA -< (w, r, wVal, d)

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

getMicroInstructions :: Instruction -> Vector [MicroInstruction]
getMicroInstructions i = case i of
  Halt -> mempty
  _ -> fetchInstruction <> case i of
    LoadA _ -> loadA
    StoreA _ -> storeA
    Add _ -> addI
    NoOp -> mempty
    Out -> outI
    Jump _ -> V.fromList [[InstructionRegisterOut, JumpFlag]]

instructions :: [Instruction]
instructions = [LoadA 5, Add 5, Out, StoreA 5, Jump 0]
initialMemory :: Memory
initialMemory = V.fromList $ (encodeInstruction <$> instructions) <> [1]

signal :: SF a (Data, Address, Instruction, [MicroInstruction])
signal = proc _ -> do
  c <- clock 1.0 -< ()
  microN <- microCounter -< c
  rec
    let instructionList' = getMicroInstructions (decodeInstruction instruction)
    instructionList <- iPre (getMicroInstructions NoOp) -< instructionList' --provide base case
    let microInsts = fromMaybe [] $ instructionList V.!? microN
    let enabled = (`elem` microInsts)
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

  returnA -< (out, n, decodeInstruction instruction, microInsts)

class ShowLevel a where
  nLevel :: a -> Int
  showLevel :: a -> String
  showLevel a = "|" <> replicate (nLevel a) '#'

instance ShowLevel Bool where
  nLevel False = 1
  nLevel True = 6

instance ShowLevel Int where
  nLevel 0 = 0
  nLevel n = n


showTime :: Time -> String
showTime = printf "%.2f"

showWithTime :: (a -> String) -> (Time, a) -> String
showWithTime f (t, a) = showTime t <> ": " <> f a

test :: IO ()
test = do
  putStr . unlines . fmap (showWithTime show) $ embed (time &&& signal) $ deltaEncode 0.5 (replicate 400 False)

main = test