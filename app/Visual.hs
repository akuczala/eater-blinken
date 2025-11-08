{-# LANGUAGE Arrows #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visual where

import Colors
import Control.Monad (when, zipWithM_)
import Control.Monad.IO.Class (MonadIO)
import Cpu
import Data.Bits (testBit)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import FRP.Yampa (SF, returnA)
import Foreign.C (CInt)
import GHC.Bits (Bits)
import Instructions
import Programs
import SDL (Renderer, V2 (..), ($=))
import qualified SDL
import SDL.Primitive (fillPolygon)
import qualified SDL.Primitive
import SDLHelper (handleKeyEvent, sdlApp)
import Signals (Data, clock)

pairUp :: [a] -> [(a, a)]
pairUp (x : y : xs) = (x, y) : pairUp xs
pairUp [x] = []
pairUp [] = []

loadSegments :: IO (V.Vector (VS.Vector Float, VS.Vector Float))
loadSegments = do
  contents <- readFile "segments.csv"
  let readVal = read . T.unpack
  let readLine = VS.fromList . fmap readVal <$> T.splitOn "," . T.pack
  let ss = V.fromList . pairUp $ readLine <$> lines contents
  return ss

digitSegments :: V.Vector Word8
digitSegments =
  V.fromList
    [ 0x3F, -- 0
      0x06, -- 1
      0x5B, -- 2
      0x4F, -- 3
      0x66, -- 4
      0x6D, -- 5
      0x7D, -- 6
      0x07, -- 7
      0x7F, -- 8
      0x6F, -- 9
      0x77, -- A
      0x7C, -- B
      0x39, -- C
      0x5E, -- D
      0x79, -- E
      0x71 -- F
    ]

drawSegments :: Renderer -> Env -> Color -> V2 Int -> V2 Int -> Data -> IO ()
drawSegments renderer segments color (V2 xScale yScale) (V2 x0 y0) d = do
  zipWithM_ f (V.toList segments) dataBits
  where
    dataBits = ((digitSegments V.! fromIntegral d) `testBit`) <$> [0 .. 7]
    f (xs, ys) b = when b $ SDL.Primitive.fillPolygon renderer xs' ys' color
      where
        xs' = convert xScale x0 xs
        ys' = convert yScale y0 ys
        convert scale pos = VS.map (round . (+ fromIntegral pos) . (* fromIntegral scale))

drawDigitDisplay :: (V2 Int -> V2 Int -> Data -> IO ()) -> V2 Int -> V2 Int -> [Data] -> IO ()
drawDigitDisplay draw scale@(V2 xScale _) (V2 x0 y0) = zipWithM_ f [0 ..]
  where
    f i = draw scale (V2 (x0 + i * (xScale + xSep)) y0)
    xSep = round (fromIntegral xScale * (0.2 :: Float))

type Env = (V.Vector (VS.Vector Float, VS.Vector Float))

test :: IO ()
test = do
  segments <- loadSegments
  let env = segments
  sdlApp firstSample handleSDLEvents (output env) signal

signal :: SF Frame (Frame, Bool, CPUState)
signal = proc frame -> do
  -- let V2 mouseX mouseY = fromIntegral <$> mousePos frame
  c <- clock 0.05 -< ()
  -- let c = spacePressed frame
  s <- cpuSignal (encodeProgram countDownStop2) -< c
  returnA -< (frame, c, s)

firstSample :: IO Frame
firstSample = do
  return $
    Frame
      { exit = False,
        mousePos = V2 0 0,
        spacePressed = False,
        pPressed = False
      }

data Frame = Frame
  { exit :: Bool,
    mousePos :: V2 CInt,
    spacePressed :: Bool,
    pPressed :: Bool
  }

drawBinary :: (Bits a) => Renderer -> V2 Int -> CInt -> Int -> a -> IO ()
drawBinary renderer p0 size nBits n = mapM_ drawBit [0 .. nBits - 1]
  where
    drawBit i = do
      (if n `testBit` i then SDL.fillRect else SDL.drawRect) renderer . Just $
        SDL.Rectangle (SDL.P $ fromIntegral <$> p0 + V2 (((nBits - 1) - i) * (fromIntegral size + 5)) 0) (V2 size size)

mkGrid :: Int -> Int -> V.Vector (V2 Double)
mkGrid m n =
  V.fromList
    [ V2 (i / fromIntegral m) (j / fromIntegral n)
    | j <- fromIntegral <$> [0 .. n - 1],
      i <- fromIntegral <$> [0 .. m - 1]
    ]

-- > vibe coded
toLastNDigitsBase :: (Integral a) => a -> Int -> a -> [a]
toLastNDigitsBase base width n
  | base < 2 = error "Base must be 2 or greater"
  | width < 0 = error "Width must be non-negative"
  | n < 0 = error "Input number must be non-negative"
  | otherwise = go width n []
  where
    go steps x acc
      | steps <= 0 = acc
      | otherwise = go (steps - 1) (x `div` base) ((x `mod` base) : acc)

-- <

lightRadius :: (Integral a) => a
lightRadius = 10

bitBoxSize :: (Integral a) => a
bitBoxSize = 20

drawLight :: Renderer -> V2 Int -> Color -> Bool -> IO ()
drawLight renderer pos color enable = do
  SDL.Primitive.circle renderer (fromIntegral <$> pos) lightRadius white
  when enable $
    SDL.Primitive.fillCircle renderer (fromIntegral <$> pos) lightRadius color

output :: Env -> Renderer -> Bool -> (Frame, Bool, CPUState) -> IO Bool
output env renderer _ (frame, c, cpuState) = do
  when (pPressed frame) $ do
    print cpuState
    print (decodeInstruction $ cpuInstruction cpuState)

  let enabled controlSignal = controlSignal `elem` cpuMicro cpuState
  SDL.rendererDrawColor renderer $= black
  SDL.clear renderer

  let dotOffset = V2 (-25) 10

  let gridPoints = fmap round . (+ V2 50 50) . (* V2 800 600) <$> mkGrid 3 3
  SDL.rendererDrawColor renderer $= cyan
  drawBinary renderer (gridPoints V.! 1) bitBoxSize 8 (cpuBus cpuState)

  -- A
  let aluPos = gridPoints V.! 5
  let aPos = aluPos + V2 0 (-60)
  SDL.rendererDrawColor renderer $= red
  drawBinary renderer aPos bitBoxSize 8 (cpuA cpuState)

  drawLight renderer (aPos + dotOffset) red (enabled ARegisterIn)
  drawLight renderer (aPos + dotOffset) green (enabled ARegisterOut)

  -- B
  let bPos = aluPos + V2 0 60
  SDL.rendererDrawColor renderer $= orange
  drawBinary renderer bPos bitBoxSize 8 (cpuB cpuState)

  drawLight renderer (bPos + dotOffset) red (enabled BRegisterIn)

  -- Alu
  SDL.rendererDrawColor renderer $= magenta
  drawBinary renderer aluPos bitBoxSize 8 (cpuAlu cpuState)

  drawLight renderer (aluPos + dotOffset) green (enabled ALUOut)
  drawLight renderer (((`div` 2) <$> aluPos + bPos) + dotOffset) blue (enabled Subtract)

  -- flags
  let flagPos = gridPoints V.! 4
  SDL.rendererDrawColor renderer $= orange
  drawBinary renderer flagPos bitBoxSize 2 (encodeFlags $ cpuFlags cpuState)
  drawLight renderer (flagPos + dotOffset) red (enabled FlagRegisterIn)

  -- out
  let outPos = gridPoints V.! 8
  SDL.rendererDrawColor renderer $= white
  drawBinary renderer outPos bitBoxSize 8 (cpuOut cpuState)

  drawLight renderer (outPos + dotOffset) red (enabled OutRegisterIn)

  drawDigitDisplay (drawSegments renderer env cyan) (pure 24) (outPos + V2 0 (bitBoxSize * 2)) (toLastNDigitsBase 10 3 (cpuOut cpuState))

  -- address
  let addrPos = gridPoints V.! 3
  SDL.rendererDrawColor renderer $= yellow
  drawBinary renderer addrPos bitBoxSize 4 (cpuAddr cpuState)

  drawLight renderer (addrPos + dotOffset) red (enabled MemoryAddressIn)

  -- memory IO
  let memPos = addrPos + V2 0 60
  SDL.rendererDrawColor renderer $= red
  drawBinary renderer memPos bitBoxSize 8 (cpuMemory cpuState)

  drawLight renderer (memPos + dotOffset) red (enabled RAMIn)
  drawLight renderer (memPos + dotOffset) green (enabled RAMOut)

  drawDigitDisplay (drawSegments renderer env red) (pure 24) (memPos + V2 0 (bitBoxSize * 2)) (toLastNDigitsBase 16 2 (cpuMemory cpuState))

  -- upper 4 instruction
  let instPos = gridPoints V.! 7

  SDL.rendererDrawColor renderer $= blue
  drawBinary renderer instPos bitBoxSize 4 (cpuInstruction cpuState `div` 16)

  -- lower 4 instruction

  SDL.rendererDrawColor renderer $= yellow
  drawBinary renderer (instPos + V2 ((fromIntegral bitBoxSize + 5) * 4) 0) bitBoxSize 4 (cpuInstruction cpuState)

  drawLight renderer (instPos + dotOffset) red (enabled InstructionRegisterIn)
  drawLight renderer (instPos + dotOffset) green (enabled InstructionRegisterOut)

  -- program counter
  let progPos = gridPoints V.! 2
  SDL.rendererDrawColor renderer $= green
  drawBinary renderer progPos bitBoxSize 4 (cpuCounter cpuState)

  drawLight renderer (progPos + dotOffset) green (enabled CounterOut)
  drawLight renderer (progPos + dotOffset) red (enabled JumpSignal)

  -- micro counter
  SDL.rendererDrawColor renderer $= blue
  drawBinary renderer (progPos + V2 ((fromIntegral bitBoxSize + 5) * 4) 0) bitBoxSize 3 (cpuMicroCounter cpuState)

  SDL.present renderer
  return (exit frame)

handleSDLEvents :: (MonadIO m) => m (Maybe Frame)
handleSDLEvents = do
  events <- SDL.pollEvents
  (SDL.P p) <- SDL.getAbsoluteMouseLocation
  return $
    Just
      Frame
        { exit = any (handleKeyEvent SDL.Pressed SDL.KeycodeQ) events,
          mousePos = p,
          spacePressed = any (handleKeyEvent SDL.Pressed SDL.KeycodeSpace) events,
          pPressed = any (handleKeyEvent SDL.Pressed SDL.KeycodeP) events
        }
