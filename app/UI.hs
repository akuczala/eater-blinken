{-# LANGUAGE OverloadedStrings #-}

module UI
  ( loadSegments,
    drawSegments,
    drawDigitDisplay,
    drawBinary,
    SegmentData,
    toLastNDigitsBase,
  )
where

import Colors
import Control.Monad (when, zipWithM_)
import Data.Bits (Bits, testBit)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.C (CInt)
import SDL (Renderer, V2 (..))
import qualified SDL
import SDL.Primitive (fillPolygon)
import Signals (Data)

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

type SegmentData = (V.Vector (VS.Vector Float, VS.Vector Float))

drawSegments :: Renderer -> SegmentData -> Color -> V2 Int -> V2 Int -> Data -> IO ()
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

drawBinary :: (Bits a) => Renderer -> V2 Int -> CInt -> Int -> a -> IO ()
drawBinary renderer p0 size nBits n = mapM_ drawBit [0 .. nBits - 1]
  where
    drawBit i = do
      (if n `testBit` i then SDL.fillRect else SDL.drawRect) renderer . Just $
        SDL.Rectangle (SDL.P $ fromIntegral <$> p0 + V2 (((nBits - 1) - i) * (fromIntegral size + 5)) 0) (V2 size size)

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
