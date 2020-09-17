module Chip8DisplayWidget
  ( c8Display
  ) where

import Chip8
import SDL
import SDL.Font (Color)
import Mortar.Drawable
import Foreign.C.Types (CInt(..))

type PixelSize = CInt

c8Display :: PixelSize -> Color -> Chip8 -> Drawable
c8Display s color c8 = widget (width,Fixed) (height,Fixed) r
  where c8w     = fst $ screensize c8
        c8h     = snd $ screensize c8
        width   = return $ fromIntegral $ c8w * fromIntegral s
        height  = return $ fromIntegral $ c8h * fromIntegral s
        r       = drawDisplay s color c8

drawPixel :: Renderer -> PixelSize -> ((CInt,CInt),Bool) -> IO ()
drawPixel _ _ ((_,_),False) = return ()
drawPixel r s ((x,y),True)  = fillRect r rect
  where rect  = Just $ Rectangle (P (V2 (x*s) (y*s))) (V2 s s)

drawDisplay :: PixelSize -> Color -> Chip8 -> Renderer -> IO ()
drawDisplay s color c8 r = do
  viewport <- get $ rendererViewport r
  case viewport of
    Just (Rectangle _ (V2 w h)) -> do
      rendererDrawColor r $= color
      mapM_ (drawPixel r s) xysp
  where xys   = [(x,y) | y <- take h [0..], x <- take w [0..]]
        w     = fromIntegral $ fst $ screensize c8
        h     = fromIntegral $ snd $ screensize c8
        xysp  = zip xys $ screen c8
