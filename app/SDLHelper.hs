{-# LANGUAGE OverloadedStrings #-}

module SDLHelper (sdlApp, handleKeyEvent) where

import Data.IORef (newIORef, readIORef, writeIORef)
import FRP.Yampa (SF, reactimate)
import GHC.IORef (IORef)
import qualified SDL

sdlApp ::
  IO a ->
  IO (Maybe a) ->
  (SDL.Renderer -> Bool -> b -> IO Bool) ->
  SF a b ->
  IO ()
sdlApp firstSample eventHandler output signal = do
  SDL.initializeAll
  window <- SDL.createWindow "Eater blinken" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  t <- SDL.time
  tRef <- newIORef t

  reactimate firstSample (input eventHandler tRef) (output renderer) signal

  SDL.destroyWindow window

updateTime :: IORef Double -> IO Double
updateTime tRef = do
  t' <- readIORef tRef
  t <- SDL.time
  writeIORef tRef t
  return $ t - t'

input :: IO a -> IORef Double -> Bool -> IO (Double, a)
input eventHandler tRef _ = do
  result <- eventHandler
  dt <- updateTime tRef
  return (dt, result)

handleKeyEvent :: SDL.InputMotion -> SDL.Keycode -> SDL.Event -> Bool
handleKeyEvent motion keycode event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyboardEvent ->
    SDL.keyboardEventKeyMotion keyboardEvent == motion
      && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
  _ -> False
