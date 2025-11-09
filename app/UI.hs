module UI
  ( drawConnection,
    drawUIElement,
    Pos,
    UIElementF (..),
    UIElement,
    DigitMode (..),
    bitBoxSize,
  )
where

import Colors
import Control.Monad (forM_, when, zipWithM_)
import Cpu (CPUState (..))
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bits (Bits, testBit)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.C (CInt)
import Instructions (ControlSignal)
import SDL (Renderer, V2 (..), ($=))
import qualified SDL
import qualified SDL.Primitive
import Signals (Data)

segmentError :: (Integral a) => a
segmentError = 17

segmentErrorCode :: Word8
segmentErrorCode = 0x09 -- show top + bottom segments only for any "invalid" digit

digitToSegments :: (Integral a) => a -> Word8
digitToSegments i = case i of
  0 -> 0x3F
  1 -> 0x06
  2 -> 0x5B
  3 -> 0x4F
  4 -> 0x66
  5 -> 0x6D
  6 -> 0x7D
  7 -> 0x07
  8 -> 0x7F
  9 -> 0x6F
  10 -> 0x77
  11 -> 0x7C
  12 -> 0x39
  13 -> 0x5E
  14 -> 0x79
  15 -> 0x71
  e | e == segmentError -> segmentError
  _ -> segmentErrorCode

type SegmentData = (V.Vector (VS.Vector Float, VS.Vector Float))

both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f

{- ORMOLU_DISABLE -}
segmentShapes :: SegmentData
segmentShapes = V.fromList . fmap (both VS.fromList) $
  [([0.8527, 0.733 , 0.2684, 0.1462, 0.2684, 0.733 , 0.8527],
    [0.122 , 0.2441, 0.2441, 0.122 , 0.0   , 0.0   , 0.122 ]),

   ([0.878 , 1.0   , 1.0   , 0.878 , 0.7559, 0.7559, 0.878 ],
    [0.1448, 0.2645, 0.6189, 0.7411, 0.6189, 0.2645, 0.1448]),

   ([0.878 , 1.0   , 1.0   , 0.878 , 0.7559, 0.7559, 0.878 ],
    [0.7909, 0.9107, 1.265 , 1.3872, 1.265 , 0.9107, 0.7909]),

   ([0.8527, 0.733 , 0.2684, 0.1462, 0.2684, 0.733 , 0.8527],
    [1.4099, 1.5319, 1.5319, 1.4099, 1.2879, 1.2879, 1.4099]),

   ([0.122 , 0.2441, 0.2441, 0.122 , 0.0   , 0.0   , 0.122 ],
    [0.7909, 0.9107, 1.265 , 1.3872, 1.265 , 0.9107, 0.7909]),

   ([0.122 , 0.2441, 0.2441, 0.122 , 0.0   , 0.0   , 0.122 ],
    [0.1448, 0.2645, 0.6189, 0.7411, 0.6189, 0.2645, 0.1448]),

   ([0.8527, 0.733 , 0.2684, 0.1462, 0.2684, 0.733 , 0.8527],
    [0.7638, 0.8858, 0.8858, 0.7638, 0.6417, 0.6417, 0.7638])]
{- ORMOLU_ENABLE -}

drawSegments :: Renderer -> Color -> V2 Int -> V2 Int -> Word8 -> IO ()
drawSegments renderer color (V2 xScale yScale) (V2 x0 y0) d = do
  zipWithM_ f (V.toList segmentShapes) dataBits
  where
    dataBits = (digitToSegments d `testBit`) <$> [0 .. 7]
    f (xs, ys) b = when b $ SDL.Primitive.fillPolygon renderer xs' ys' color
      where
        xs' = convert xScale x0 xs
        ys' = convert yScale y0 ys
        convert scale pos = VS.map (round . (+ fromIntegral pos) . (* fromIntegral scale))

drawDigitDisplay :: (V2 Int -> V2 Int -> Data -> IO ()) -> V2 Int -> V2 Int -> [Word8] -> IO ()
drawDigitDisplay draw scale@(V2 xScale _) (V2 x0 y0) = zipWithM_ f [0 ..]
  where
    f i = draw scale (V2 (x0 + i * (xScale + xSep)) y0)
    xSep = round (fromIntegral xScale * (0.2 :: Float))

drawBinary :: (Bits a) => Renderer -> V2 Int -> CInt -> Int -> a -> IO ()
drawBinary renderer p0 size nBits n = mapM_ drawBit [0 .. nBits - 1]
  where
    drawBit i = do
      (if n `testBit` i then SDL.fillRect else SDL.drawRect) renderer . Just $
        SDL.Rectangle (SDL.P $ fromIntegral <$> p0 + V2 (((nBits - 1) - i) * (fromIntegral size + 5)) 0) (V2 size size)

-- > vibe coded
toLastNDigitsBase :: (Integral a, Integral b) => a -> Int -> a -> Maybe [b]
toLastNDigitsBase base width n
  | base < 2 = Nothing
  | width < 0 = Nothing
  | n < 0 = Nothing
  | otherwise = Just . fmap fromIntegral $ go width (fromIntegral n) []
  where
    go steps x acc
      | steps <= 0 = acc
      | otherwise = go (steps - 1) (x `div` base) ((x `mod` base) : acc)

-- <

drawConnection :: Renderer -> Int -> V2 Int -> V2 Int -> IO ()
drawConnection renderer xVertical (V2 x1 y1) (V2 x2 y2) = do
  let toPoint = SDL.P . fmap fromIntegral
  let drawLine v1 v2 = SDL.drawLine renderer (toPoint v1) (toPoint v2)
  drawLine (V2 x1 y1) (V2 xVertical y1)
  drawLine (V2 xVertical y1) (V2 xVertical y2)
  drawLine (V2 x2 y2) (V2 xVertical y2)

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
    let enabled = (`elem` cpuMicro cpuState)
    let lightPos = pos + V2 0 (bitBoxSize `div` 2)
    forM_ writeSignal $ drawLight renderer lightPos red . enabled
    forM_ readSignal $ drawLight renderer lightPos green . enabled
  BitLight color pos f -> drawLight renderer pos color (f cpuState)
  SegmentDisplay color pos n mode f ->
    drawDigitDisplay
      (drawSegments renderer color)
      (pure digitScale)
      pos
      (fromMaybe [segmentError] $ toLastNDigitsBase (digitModeToBase mode) n (fromIntegral $ f cpuState))
