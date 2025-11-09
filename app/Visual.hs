{-# LANGUAGE Arrows #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visual where

import Colors
import Control.Monad (when, zipWithM_)
import Control.Monad.IO.Class (MonadIO)
import Cpu
import Data.Maybe (fromMaybe)
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
import Data.Word (Word8)

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

output :: Renderer -> Bool -> (Frame, Bool, CPUState) -> IO Bool
output renderer _ (frame, c, cpuState) = do
  when (pPressed frame) $ do
    print cpuState
    print (decodeInstruction $ cpuInstruction cpuState)

  SDL.rendererDrawColor renderer $= black
  SDL.clear renderer

  drawBlinkenlights renderer cpuState

  SDL.present renderer
  return (exit frame)

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

type Pos = V2 Int

data DigitMode = DecMode | HexMode
digitModeToBase :: DigitMode -> Int
digitModeToBase mode = case mode of
  DecMode -> 10
  HexMode -> 16

data UIElementF a
  = BinaryLights Color Pos Int (a -> Word8)
  | ControlLight Pos (Maybe ControlSignal) (Maybe ControlSignal) -- write, read
  | BitLight Color Pos (a -> Bool)
  | SegmentDisplay Color Pos Int DigitMode (a -> Word8)

type UIElement = UIElementF CPUState

drawUIElement :: Renderer -> CPUState -> UIElement -> IO ()
drawUIElement renderer cpuState e = case e of
  BinaryLights color pos n f -> do
    SDL.rendererDrawColor renderer $= color
    drawBinary renderer pos bitBoxSize n (f cpuState)
  ControlLight pos writeSignal readSignal -> do
    let enabled cs = cs `elem` cpuMicro cpuState
    case writeSignal of
      (Just cs) -> drawLight renderer pos red (enabled cs)
      _ -> pure ()
    case readSignal of
      (Just cs) -> drawLight renderer pos green (enabled cs)
      _ -> pure ()
  BitLight color pos f -> drawLight renderer pos color (f cpuState)
  SegmentDisplay color pos n mode f -> drawDigitDisplay
          (drawSegments renderer color)
          (pure digitScale)
          pos
          (fromMaybe [segmentError] $ toLastNDigitsBase (digitModeToBase mode) n (fromIntegral $ f cpuState))

dotOffset :: V2 Int
dotOffset = V2 (-25) 10

busElements :: Pos -> [UIElement]
busElements p = [
  BinaryLights cyan p 8 cpuBus
  ]

aluElements :: Pos -> [UIElement]
aluElements aluPos = let
  aPos = aluPos + V2 0 (-60)
  bPos = aluPos + V2 0 60
  in
    [
      BinaryLights red aPos 8 cpuA,
      BinaryLights orange bPos 8 cpuB,
      BinaryLights yellow aluPos 8 cpuAlu,

      ControlLight (aPos + dotOffset) (Just ARegisterIn) (Just ARegisterOut),
      ControlLight (bPos + dotOffset) (Just BRegisterIn) Nothing,
      ControlLight (aluPos + dotOffset) Nothing (Just ALUOut),
      BitLight blue (((`div` 2) <$> aluPos + bPos) + dotOffset) (\s -> Subtract `elem` cpuMicro s)
      ]

flagElements :: Pos -> [UIElement]
flagElements pos = [
  BinaryLights magenta pos 2 (encodeFlags . cpuFlags),
  ControlLight (pos + dotOffset) (Just FlagRegisterIn) Nothing
  ]

outElements :: Pos -> [UIElement]
outElements pos = [
  BinaryLights white pos 8 cpuOut,
  ControlLight (pos + dotOffset) (Just OutRegisterIn) Nothing,
  SegmentDisplay cyan (pos + V2 0 (bitBoxSize * 2)) 3 DecMode cpuOut
  ]

memoryElements :: Pos -> [UIElement]
memoryElements addrPos = let
    memPos = addrPos + V2 0 60
  in [
  BinaryLights yellow addrPos 4 cpuAddr,
  ControlLight (addrPos + dotOffset) (Just MemoryAddressIn) Nothing,

  BinaryLights green memPos 8 cpuMemory,
  ControlLight (memPos + dotOffset) (Just RAMIn) (Just RAMOut),
  SegmentDisplay green (memPos + V2 0 (bitBoxSize * 2)) 2 HexMode cpuMemory
  ]

drawBlinkenlights :: Renderer -> CPUState -> IO ()
drawBlinkenlights renderer cpuState = do
  let enabled controlSignal = controlSignal `elem` cpuMicro cpuState
  let drawTemp elmts pos = mapM_ (drawUIElement renderer cpuState) (elmts pos)

  let drawBus = drawTemp busElements
  let drawAlu = drawTemp aluElements
  let drawFlags = drawTemp flagElements
  let drawOut = drawTemp outElements
  let drawMemory = drawTemp memoryElements

  let drawInstruction pos = do
        -- upper 4 instruction
        SDL.rendererDrawColor renderer $= blue
        drawBinary renderer pos bitBoxSize 4 (cpuInstruction cpuState `div` 16)

        -- lower 4 instruction

        SDL.rendererDrawColor renderer $= yellow
        drawBinary renderer (pos + V2 ((fromIntegral bitBoxSize + 5) * 4) 0) bitBoxSize 4 (cpuInstruction cpuState)

        drawLight renderer (pos + dotOffset) red (enabled InstructionRegisterIn)
        drawLight renderer (pos + dotOffset) green (enabled InstructionRegisterOut)

  let drawCounter pos = do
        -- program counter
        SDL.rendererDrawColor renderer $= green
        drawBinary renderer pos bitBoxSize 4 (cpuCounter cpuState)

        drawLight renderer (pos + dotOffset) green (enabled CounterOut)
        drawLight renderer (pos + dotOffset) red (enabled JumpSignal)

        -- micro counter
        SDL.rendererDrawColor renderer $= blue
        drawBinary renderer (pos + V2 ((fromIntegral bitBoxSize + 5) * 4) 0) bitBoxSize 3 (cpuMicroCounter cpuState)

  do
    let gridPoints = fmap round . (+ V2 50 50) . (* V2 800 600) <$> mkGrid 3 3
    let f i draw = draw (gridPoints V.! i)
    let blank _ = pure ()
{- ORMOLU_DISABLE -}
    zipWithM_ f [0..] [
      blank,      drawBus,         drawCounter,
      drawMemory, drawFlags,       drawAlu,
      blank,      drawInstruction, drawOut
      ]
{- ORMOLU_ENABLE -}
  SDL.rendererDrawColor renderer $= cyan
  drawConnection renderer 400 (V2 100 100) (V2 500 500)

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
