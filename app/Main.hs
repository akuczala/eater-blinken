module Main where
import FRP.Yampa
import Text.Printf (printf)
import Cpu (cpuSignal)

-- signal :: SF a Int
-- signal = proc x -> do
--   c <- clock 1.0 -< x
--   n <- counter -< c
--   ready <- hold False <<< after 4.4 True -< ()
--   out <- latch 0 -< (ready, n)
--   returnA -< out

-- testMemory :: SF a (Bool, Bool, Bool, Data)
-- testMemory = proc _ -> do
--   w <- (||) <$> pulse 1.0 2.0 <*> pulse 4.0 4.5-< ()
--   r <- (||) <$> pulse 2.5 3.0 <*> pulse 3.5 5.0 -< ()
--   wVal <- step 3.0 -< ()

--   d <- ram (V.fromList [1, 2]) -< (w, if r then 1 else 0, if wVal then 4 else 5)
--   returnA -< (w, r, wVal, d)


class ShowLevel a where
  nLevel :: a -> Int
  showLevel :: a -> String
  showLevel a = "|" <> replicate (nLevel a) '#'

instance ShowLevel Bool where
  nLevel False = 1
  nLevel True = 6

instance ShowLevel Int where
  nLevel 0 = 0
  nLevel n = n


showTime :: Time -> String
showTime = printf "%.2f"

showWithTime :: (a -> String) -> (Time, a) -> String
showWithTime f (t, a) = showTime t <> ": " <> f a

test :: IO ()
test = do
  putStr . unlines . fmap (showWithTime show) $ embed (time &&& cpuSignal) $ deltaEncode 0.5 (replicate 400 False)

main = test