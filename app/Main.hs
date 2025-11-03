{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Main where
import FRP.Yampa
import Text.Printf (printf)
import Data.Vector (Vector)
import qualified Data.Vector as V

type Address = Int
type Memory = Vector Int
type Data = Int

defaultData :: Data
defaultData = 0

data Instruction a where
  LoadA :: Address -> Instruction Address
  Jump :: Address -> Instruction Address
  Add :: Address -> Instruction Address
  NoOp :: Instruction ()
  Out :: Instruction ()

data MicroInstruction
  = ARegisterIn
  | ARegisterOut
  | BRegisterIn
  | ALUOut
  | OutRegisterIn
  | CounterOut
  | CounterEnable
  | MemoryAddressIn
  | RAMOut
  | InstructionRegisterIn
  | InstructionRegisterOut -- only lower 4 bits go to bus

fetchInstruction :: [[MicroInstruction]]
fetchInstruction = [
  [CounterOut, MemoryAddressIn],
  [RAMOut, InstructionRegisterIn],
  [CounterEnable]
  ]

-- does not include fetchInstruction
loadA :: [[MicroInstruction]]
loadA = [
  [InstructionRegisterOut, MemoryAddressIn],
  [RAMOut, ARegisterIn]
  ]

addI :: [[MicroInstruction]]
addI = [
  [InstructionRegisterOut, MemoryAddressIn],
  [RAMOut, BRegisterIn],
  [ALUOut, ARegisterIn]
  ]

outI :: [[MicroInstruction]]
outI = [[ARegisterOut, OutRegisterIn]]

type Register = SF (Bool, Data) Data
aRegister :: Register
aRegister = latch 0

bRegister :: Register
bRegister = latch 0


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

-- signal :: SF a Bool
-- signal = pulse 1.0 2.0
-- signal :: SF a Bool
-- signal = isEvent <$> repeatedly 1.0 ()

signal :: SF a (Bool, Data, Data, Data, Data, Data)
signal = proc _ -> do
  c <- clock 1.0 -< ()
  microInstruction <- microCounter -< c
  let addrIn = microInstruction == 0
  let countOut = microInstruction == 0
  let ramOut = microInstruction == 1
  let iIn = microInstruction == 1
  let countEnable = microInstruction == 2
  let mIn = False
  let aIn = False
  let outIn = False
  rec
    let bus
          | countOut = n
          -- | addrOut = addr
          | ramOut = m
          | otherwise = 0
    n <- counter -< countEnable && c
    instruction <- aRegister -< (iIn && c, bus)
    addr <- aRegister -< (addrIn && c, bus) -- address register
    m <- ram (V.fromList [1, 2, 3, 4]) -< (mIn, addr, bus)
    a <- aRegister -< (aIn && c, bus) -- A register
    out <- aRegister -< (outIn && c, bus) -- out register

  returnA -< (c, n, instruction, microInstruction, addr, m)

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
  putStr . unlines . fmap (showWithTime show) $ embed (time &&& signal) $ deltaEncode 0.2 (replicate 120 False)

main = test