{-# LANGUAGE Arrows #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visual (main) where

import Colors
import Control.Monad (when, zipWithM_)
import Control.Monad.IO.Class (MonadIO)
import Cpu
import qualified Data.Vector as V
import FRP.Yampa (SF, returnA)
import Foreign.C (CInt)
import Instructions
import Programs
import SDL (Renderer, V2 (..), ($=))
import qualified SDL
import SDLHelper (handleKeyEvent, sdlApp)
import Signals (clock)
import UI

main :: IO ()
main = do
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

dotOffset :: V2 Int
dotOffset = V2 (-25) 0

busElements :: Pos -> [UIElement]
busElements p =
  [ BinaryLights cyan p 8 cpuBus
  ]

aluElements :: Pos -> [UIElement]
aluElements aluPos =
  let aPos = aluPos + V2 0 (-60)
      bPos = aluPos + V2 0 60
   in [ BinaryLights red aPos 8 cpuA,
        BinaryLights orange bPos 8 cpuB,
        BinaryLights yellow aluPos 8 cpuAlu,
        ControlLight (aPos + dotOffset) (Just ARegisterIn) (Just ARegisterOut),
        ControlLight (bPos + dotOffset) (Just BRegisterIn) Nothing,
        ControlLight (aluPos + dotOffset) Nothing (Just ALUOut),
        BitLight blue (((`div` 2) <$> aluPos + bPos) + dotOffset) (\s -> Subtract `elem` cpuMicro s)
      ]

flagElements :: Pos -> [UIElement]
flagElements pos =
  [ BinaryLights magenta pos 2 (encodeFlags . cpuFlags),
    ControlLight (pos + dotOffset) (Just FlagRegisterIn) Nothing
  ]

outElements :: Pos -> [UIElement]
outElements pos =
  [ BinaryLights white pos 8 cpuOut,
    ControlLight (pos + dotOffset) (Just OutRegisterIn) Nothing,
    SegmentDisplay cyan (pos + V2 0 (bitBoxSize * 2)) 3 DecMode cpuOut
  ]

memoryElements :: Pos -> [UIElement]
memoryElements addrPos =
  let memPos = addrPos + V2 0 60
   in [ BinaryLights yellow addrPos 4 cpuAddr,
        ControlLight (addrPos + dotOffset) (Just MemoryAddressIn) Nothing,
        BinaryLights green memPos 8 cpuMemory,
        ControlLight (memPos + dotOffset) (Just RAMIn) (Just RAMOut),
        SegmentDisplay green (memPos + V2 0 (bitBoxSize * 2)) 2 HexMode cpuMemory
      ]

instructionElements :: Pos -> [UIElement]
instructionElements pos =
  [ BinaryLights blue pos 4 ((`div` 16) . cpuInstruction),
    BinaryLights yellow (pos + V2 ((bitBoxSize + 5) * 4) 0) 4 cpuInstruction,
    ControlLight (pos + dotOffset) (Just InstructionRegisterIn) (Just InstructionRegisterOut)
  ]

counterElements :: Pos -> [UIElement]
counterElements pos =
  [ BinaryLights green pos 4 cpuCounter,
    BinaryLights blue (pos + V2 ((bitBoxSize + 5) * 4) 0) 3 cpuMicroCounter,
    ControlLight (pos + dotOffset) (Just JumpSignal) (Just CounterOut)
  ]

{- ORMOLU_DISABLE -}
drawBlinkenlights :: Renderer -> CPUState -> IO ()
drawBlinkenlights renderer cpuState = do
  do
    let gridPoints = fmap round . (+ V2 50 50) . (* V2 800 600) <$> mkGrid 3 3
    let draw i elmts = mapM_ (drawUIElement renderer cpuState) (elmts (gridPoints V.! i))
    let blank _ = []
    let elements = [
    
          blank,          busElements,         counterElements,
          memoryElements, flagElements,        aluElements,
          blank,          instructionElements, outElements
          ]
    zipWithM_ draw [0..] elements
  SDL.rendererDrawColor renderer $= cyan
  drawConnection renderer 400 (V2 100 100) (V2 500 500)
{- ORMOLU_ENABLE -}

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
