{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  , appLoop
  ) where

import AppState

import Mortar
import SDL
import qualified SDL.Font as F
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Foreign.C.Types (CInt(..))

app :: F.Font -> App AppState
app f = App { appWidgets        = c8Widgets
            , appEventsHandler  = c8Events
            , width             = 0
            , height            = 0
            , appQuit           = False
            , state             = defaultAppState f
            }

c8Widgets :: AppState -> [Drawable]
c8Widgets s = [label (appfont s) (V4 255 255 255 0) (V4 0 0 0 0) "Coming soon ..."]

c8Events :: App AppState -> [Event] -> IO (App AppState)
c8Events a []     = return a
c8Events a (e:es) = do
  a' <- c8Event a e
  c8Events a' es

c8Event :: App AppState -> Event -> IO (App AppState)
c8Event a e = c8EventPayload a (eventPayload e)

c8EventPayload :: App AppState -> EventPayload -> IO (App AppState)
c8EventPayload a (WindowSizeChangedEvent d) = do
  let WindowSizeChangedEventData _ wh = d
  let V2 w h = wh
  return a { width = w, height = h }

c8EventPayload a (WindowClosedEvent d) = do
  let WindowClosedEventData w = d
  destroyWindow w
  return a { appQuit = True }

c8EventPayload a (KeyboardEvent d) = do
  if (keyboardEventKeyMotion d == Pressed && keysymKeycode (keyboardEventKeysym d) == KeycodeQ) then return a { appQuit = True } else return a

c8EventPayload a _ = return a

drawWidget :: Renderer -> Rectangle CInt -> Drawable -> IO ()
drawWidget r rect w = do
  rendererViewport r $= Just rect
  render r w

appLoop :: App AppState -> Renderer -> IO ()
appLoop a r = do
  events <- pollEvents

  -- clear renderer
  rendererViewport r $= Nothing
  rendererDrawColor r $= V4 0 0 0 255
  clear r

  a' <- (appEventsHandler a) a events
  let w     = CInt $ width a'
  let h     = CInt $ height a'
  let rect  = Rectangle (P (V2 0 0)) (V2 w h)
  mapM_ (drawWidget r rect) $ appWidgets a' (state a')

  present r
  threadDelay 16000

  unless (appQuit a') (appLoop a' r)

