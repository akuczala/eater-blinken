module Programs where

import Instructions
import Signals

doublingProgram :: Address -> [Instruction]
doublingProgram x = [LoadI 1, StoreA x, LoadA x, Add x, Out, StoreA x, Jump 0]

fibProgram :: Address -> Address -> [Instruction]
fibProgram x y =
  [ LoadI 1,
    StoreA x,
    StoreA y,
    LoadA x,
    Add y,
    Out,
    StoreA x,
    Add y,
    Out,
    StoreA y,
    Jump 3
  ]

countDown :: Address -> Address -> [Instruction]
countDown x y =
  [ LoadI 1,
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

countDownStop :: Address -> Address -> [Instruction]
countDownStop x y =
  [ LoadI 1,
    StoreA x,
    LoadI 5,
    StoreA y,
    Sub x, -- 4
    Out,
    JEZ 8,
    Jump 4,
    Halt -- 8
  ]
