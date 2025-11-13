module Programs where

import Data.Word (Word8)
import Instructions
import Signals

doublingProgram :: Address -> [Instruction]
doublingProgram x = [LoadI 1, StoreA x, LoadA x, Add x, Out, StoreA x, Jump 0]

fibProgram :: Address -> [Instruction]
fibProgram x =
  let y = x + 1
   in [ LoadI 0,
        StoreA x,
        LoadI 1,
        StoreA y,
        LoadA x, -- 4
        Add y,
        Out,
        StoreA x,
        Add y,
        Out,
        StoreA y,
        Jump 4
      ]

countDown :: Address -> [Instruction]
countDown x =
  let y = x + 1
   in [ LoadI 1,
        StoreA x,
        LoadI 3,
        StoreA y,
        Sub x, -- 4
        Out,
        JEZ 8,
        Jump 4,
        LoadI 1, -- 8
        Add y,
        StoreA y,
        Jump 4
      ]

countDownStop :: Address -> [Instruction]
countDownStop x =
  let y = x + 1
   in [ LoadI 1,
        StoreA x,
        LoadI 5,
        StoreA y,
        Sub x, -- 4
        Out,
        JEZ 8,
        Jump 4,
        Halt -- 8
      ]

countDownStop2 :: Address -> [Instruction]
countDownStop2 x =
  [ LoadI 0xF,
    StoreA x,
    Dec, -- 2
    Out,
    JEZ 6,
    Jump 2,
    Jump 0 -- 6
  ]

compareProgram :: Address -> [Instruction]
compareProgram x =
  [ LoadI 7,
    StoreA x,
    LoadI 5,
    Sub x,
    JLZ 6,
    Halt, -- 5
    LoadI 1, -- 6
    Out,
    Halt
  ]

multiplyProgram :: Word8 -> Word8 -> Address -> [Instruction]
multiplyProgram a n i =
  let z = i + 1
      endAddr = 0xB
      decAddr = 0x3
   in [ LoadI 0,
        StoreA z, -- initialize z to 0
        LoadI n,
        JEZ endAddr, -- 3
        StoreA i,
        LoadA z,
        AddI a,
        StoreA z,
        LoadA i,
        Dec,
        Jump decAddr, -- A
        LoadA z, -- B
        Out, -- C
        Halt -- D
        -- i @ E
        -- z @ F
      ]
