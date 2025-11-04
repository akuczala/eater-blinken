{-# LANGUAGE OverloadedStrings #-}

module SDLHelper (sdlApp, anyExitEvent) where

import qualified SDL
import FRP.Yampa (SF, reactimate)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.IORef (IORef)

sdlApp :: IO a
  -> IO (Maybe a)
  -> (SDL.Renderer -> Bool -> b -> IO Bool)
  -> SF a b
  -> IO ()
sdlApp firstSample eventHandler output signal = do
  SDL.initializeAll
  window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
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

anyExitEvent :: Foldable t0 => t0 SDL.Event -> Bool
anyExitEvent events =
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
  in any eventIsQPress events