module Buffered (main) where

import Codec.Picture (Pixel (componentCount), PixelRGBA8)
import Data.Function ((&))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Graphics.GL (glDrawBuffer, glReadBuffer, glReadPixels, pattern GL_BACK, pattern GL_RGBA, pattern GL_UNSIGNED_BYTE)
import Graphics.Gloss
  ( BitmapFormat (..),
    Color,
    Display (InWindow),
    Picture (Color, Pictures, Scale, Translate),
    PixelFormat (PxRGBA),
    RowOrder (BottomToTop),
    bitmapOfForeignPtr,
    blue,
    circleSolid,
    makeColor,
    withAlpha,
  )
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import qualified Graphics.Gloss.Rendering as Gloss
import Prelude

main :: IO ()
main = do
  s <- Gloss.initState
  buffer <- newBuffer width height
  simulateIO
    displayMode
    backgroundColor
    simResolution
    (initialModel buffer)
    displayModel
    (updateModel s)

data Model = Model
  { pos :: Float,
    trace :: Buffer
  }

width :: Num n => n
width = 400

height :: Num n => n
height = 50

displayMode :: Display
displayMode = InWindow "buffered" (width, height) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 1 1 1 0

simResolution :: Num n => n
simResolution = 15

initialModel :: Buffer -> Model
initialModel buffer =
  Model
    { pos = - width / 20,
      trace = buffer
    }

displayModel :: Applicative m => Model -> m Picture
displayModel = pure . displayBuffer . trace

ball :: Float -> Picture
ball x =
  circleSolid 1
    & Translate x 0
    & Scale 10 10
    & Color (withAlpha 0.5 blue)

updateModel :: Gloss.State -> viewPort -> Float -> Model -> IO Model
updateModel state _ t Model {pos, trace} = do
  trace' <- updateBuffer state (ball pos) trace

  pure
    Model
      { pos = pos + simResolution * t,
        trace = trace'
      }

data Buffer = Buffer Int Int (ForeignPtr Word8)

newBuffer :: Int -> Int -> IO Buffer
newBuffer w h = do
  Buffer width height
    <$> mallocPlainForeignPtrBytes @Word8 (w * h * bytesPerPixel)

displayBuffer :: Buffer -> Picture
displayBuffer buffer =
  bitmapOfForeignPtr w h format fptr False
  where
    Buffer w h fptr = buffer

updateBuffer :: Gloss.State -> Picture -> Buffer -> IO Buffer
updateBuffer state picture buffer = do
  drawToBuffer state (Pictures [displayBuffer buffer, picture]) buffer
  pure buffer

drawToBuffer :: Gloss.State -> Picture -> Buffer -> IO ()
drawToBuffer state picture buffer = do
  glDrawBuffer GL_BACK

  Gloss.withClearBuffer (makeColor 1 1 1 1) do
    Gloss.withModelview (w, h) do
      Gloss.renderPicture state 1 picture
  glReadBuffer GL_BACK

  withForeignPtr fptr $
    glReadPixels 0 0 (i w) (i h) pixelFormat pixelType
  where
    i = fromIntegral
    pixelFormat = GL_RGBA
    pixelType = GL_UNSIGNED_BYTE
    Buffer w h fptr = buffer

-- # Constants

bytesPerPixel :: Int
bytesPerPixel = componentCount (undefined :: PixelRGBA8)

format :: BitmapFormat
format =
  BitmapFormat
    { rowOrder = BottomToTop,
      pixelFormat = PxRGBA
    }
