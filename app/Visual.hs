{-# LANGUAGE Arrows #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visual where

import Colors
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Cpu
import qualified Data.Vector as V
import FRP.Yampa (SF, returnA)
import Foreign.C (CInt)
import Instructions
import Programs
import SDL (Renderer, V2 (..), ($=))
import qualified SDL
import qualified SDL.Primitive
import SDLHelper (handleKeyEvent, sdlApp)
import Signals (clock)
import UI
import Data.Maybe (fromMaybe)

test :: IO ()
test = do
  sdlApp firstSample handleSDLEvents output signal

signal :: SF Frame (Frame, Bool, CPUState)
signal = proc frame -> do
  -- let V2 mouseX mouseY = fromIntegral <$> mousePos frame
  c <- clock 0.1 -< ()
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

mkGrid :: Int -> Int -> V.Vector (V2 Double)
mkGrid m n =
  V.fromList
    [ V2 (i / fromIntegral m) (j / fromIntegral n)
    | j <- fromIntegral <$> [0 .. n - 1],
      i <- fromIntegral <$> [0 .. m - 1]
    ]

lightRadius :: (Integral a) => a
lightRadius = 10

bitBoxSize :: (Integral a) => a
bitBoxSize = 20

digitScale :: (Integral a) => a
digitScale = 32

drawLight :: Renderer -> V2 Int -> Color -> Bool -> IO ()
drawLight renderer pos color enable = do
  SDL.Primitive.circle renderer (fromIntegral <$> pos) lightRadius white
  when enable $
    SDL.Primitive.fillCircle renderer (fromIntegral <$> pos) lightRadius color

type Env = SegmentData

output :: Renderer -> Bool -> (Frame, Bool, CPUState) -> IO Bool
output renderer _ (frame, c, cpuState) = do
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

  drawDigitDisplay
    (drawSegments renderer cyan)
    (pure digitScale)
    (outPos + V2 0 (bitBoxSize * 2))
    (fromMaybe [segmentError] $ toLastNDigitsBase 10 3 (cpuOut cpuState))

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

  drawDigitDisplay
    (drawSegments renderer red)
    (pure digitScale)
    (memPos + V2 0 (bitBoxSize * 2))
    (fromMaybe [segmentError] $ toLastNDigitsBase 16 2 (cpuMemory cpuState))

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
