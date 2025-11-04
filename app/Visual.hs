{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}

module Visual where

import qualified SDL
import SDL (Renderer, V2(..), ($=))
import FRP.Yampa (SF, returnA)
import Colors
import Foreign.C (CInt)
import Control.Monad.IO.Class (MonadIO)
import SDLHelper (sdlApp)
import Cpu (CPUState (..), cpuSignal, cpuOut)
import Signals (clock)
import Data.Bits (testBit)
import qualified Data.Vector as V

test :: IO ()
test = sdlApp firstSample handleSDLEvents output signal

signal :: SF Frame (Frame, Bool, CPUState)
signal = proc frame -> do
  -- let V2 mouseX mouseY = fromIntegral <$> mousePos frame
  c <- clock 0.2 -< ()
  s <- cpuSignal -< c
  returnA -< (frame, c, s)

firstSample :: IO Frame
firstSample = do
  return $ Frame {exit = False, mousePos = V2 0 0}

data Frame = Frame {exit :: Bool, mousePos :: V2 CInt}

drawBinary :: Renderer -> V2 Int -> CInt -> Int -> Int -> IO ()
drawBinary renderer p0 size nBits n = mapM_ drawBit [0 .. nBits - 1] where
  drawBit i = do
      (if n `testBit` i then SDL.fillRect else SDL.drawRect) renderer . Just $
        SDL.Rectangle (SDL.P $ fromIntegral <$> p0 + V2 (((nBits - 1) - i) * (fromIntegral size + 5)) 0) (V2 size size)

mkGrid :: Int -> Int -> V.Vector (V2 Double)
mkGrid m n = V.fromList [
  V2 (i/ fromIntegral m) (j / fromIntegral n)
  | j <- fromIntegral <$> [0 .. n - 1], i <- fromIntegral <$> [0 .. m - 1]
  ]

output :: Renderer -> Bool -> (Frame, Bool, CPUState) -> IO Bool
output renderer _ (frame, c, cpuState) = do
  SDL.rendererDrawColor renderer $= black
  SDL.clear renderer

  let bitBoxSize = 20
  let gridPoints = fmap round . (+ V2 0 50) . (* V2 800 600) <$> mkGrid 3 3
  SDL.rendererDrawColor renderer $= cyan
  drawBinary renderer (gridPoints V.! 1) bitBoxSize 8 (cpuBus cpuState)

  -- A
  SDL.rendererDrawColor renderer $= red
  drawBinary renderer (gridPoints V.! 5 + V2 0 (-60)) bitBoxSize 8 (cpuA cpuState)

  -- B
  SDL.rendererDrawColor renderer $= orange
  drawBinary renderer (gridPoints V.! 5 + V2 0 60) bitBoxSize 8 (cpuB cpuState)

  -- Alu
  SDL.rendererDrawColor renderer $= magenta
  drawBinary renderer (gridPoints V.! 5) bitBoxSize 8 (cpuAlu cpuState)

  -- out
  SDL.rendererDrawColor renderer $= yellow
  drawBinary renderer (gridPoints V.! 8) bitBoxSize 8 (cpuOut cpuState)

  -- address
  SDL.rendererDrawColor renderer $= yellow
  drawBinary renderer (gridPoints V.! 3) bitBoxSize 4 (cpuAddr cpuState)

  -- memory IO
  SDL.rendererDrawColor renderer $= red
  drawBinary renderer (gridPoints V.! 3 + V2 0 60) bitBoxSize 8 (cpuMemory cpuState)

  -- upper 4 instruction
  SDL.rendererDrawColor renderer $= blue
  drawBinary renderer (gridPoints V.! 7) bitBoxSize 4 (cpuInstruction cpuState `div` 16)

  -- lower 4 instruction
  SDL.rendererDrawColor renderer $= yellow
  drawBinary renderer (gridPoints V.! 7 + V2 ((fromIntegral bitBoxSize + 5) * 4) 0) bitBoxSize 4 (cpuInstruction cpuState)

  -- program counter
  SDL.rendererDrawColor renderer $= green
  drawBinary renderer (gridPoints V.! 2) bitBoxSize 4 (cpuCounter cpuState)

  -- micro counter
  SDL.rendererDrawColor renderer $= blue
  drawBinary renderer (gridPoints V.! 2 + V2 ((fromIntegral bitBoxSize + 5) * 4) 0) bitBoxSize 3 (cpuMicroCounter cpuState)

  SDL.present renderer
  return (exit frame)



handleSDLEvents :: MonadIO m => m (Maybe Frame)
handleSDLEvents = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  (SDL.P p) <- SDL.getAbsoluteMouseLocation
  return $ Just Frame {exit = qPressed, mousePos = p}
