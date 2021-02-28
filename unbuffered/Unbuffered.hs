module Unbuffered (main) where

import Data.Function ((&))
import Graphics.Gloss
  ( Color,
    Display (InWindow),
    Picture (Color, Pictures, Scale, Translate),
    blue,
    circleSolid,
    makeColor,
    withAlpha,
  )
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)

main :: IO ()
main = do
  simulateIO
    displayMode
    backgroundColor
    simResolution
    initialModel
    displayModel
    updateModel

data Model = Model
  { pos :: Float,
    trace :: [Picture]
  }

width :: Num n => n
width = 400

height :: Num n => n
height = 50

displayMode :: Display
displayMode = InWindow "unbuffered" (width, height) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 1 1 1 0

simResolution :: Num n => n
simResolution = 15

initialModel :: Model
initialModel =
  Model
    { pos = - width / 20,
      trace = []
    }

displayModel :: Applicative m => Model -> m Picture
displayModel = pure . Pictures . trace

ball :: Float -> Picture
ball x =
  circleSolid 1
    & Translate x 0
    & Scale 10 10
    & Color (withAlpha 0.5 blue)

updateModel :: viewPort -> Float -> Model -> IO Model
updateModel _ t Model {pos, trace} =
  pure
    Model
      { pos = pos + simResolution * t,
        trace = trace ++ [ball pos]
      }
