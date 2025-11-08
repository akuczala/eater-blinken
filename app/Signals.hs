{-# LANGUAGE Arrows #-}

module Signals where

import Data.Vector
import FRP.Yampa
import Data.Word (Word8)

type Address = Data

type Memory = Vector Data

type Data = Word8

defaultData :: Data
defaultData = 0

step :: Time -> SF a Bool
step t1 = unEdge <<< after t1 ()

pulse :: Time -> Time -> SF a Bool
pulse t1 t2 = proc x -> do
  b1 <- unEdge <<< after t1 () -< x
  b2 <- unEdge <<< after t2 () -< x
  returnA -< b1 && not b2

clock :: Time -> SF a Bool
clock t = accumHoldBy s False <<< repeatedly t ()
  where
    s x _ = not x

counter :: (Num a) => SF Bool a
counter = edge >>> accumHoldBy (\b _ -> b + 1) 0

cycleCounter :: (Integral a) => a -> SF Bool a
cycleCounter resetAt = edge >>> accumHoldBy (\b _ -> (b + 1) `mod` resetAt) 0

fLatch :: (b -> a -> b) -> b -> SF (Bool, a) b
fLatch f init_ = stuff >>> accumHoldBy f init_
  where
    stuff = proc (c, a) -> do
      e <- edge -< c
      returnA -< fmap (const a) e

latch :: a -> SF (Bool, a) a
latch init_ = stuff >>> accumHoldBy (\_ a -> a) init_
  where
    stuff = proc (c, a) -> do
      e <- edge -< c
      returnA -< fmap (const a) e

unEdge :: SF (Event ()) Bool
unEdge = arr (fmap (const not)) >>> accumHold False
