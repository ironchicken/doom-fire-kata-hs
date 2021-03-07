{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Word (Word8)
import Foreign.C.Types
import SDL
import System.Random (StdGen, getStdGen, randomIO)

data FireStrand
  = FireStrand
  { fireWhite :: CInt
  , fireRed1 :: CInt
  , fireRed2 :: CInt
  , fireRed3 :: CInt
  , fireRed4 :: CInt
  , fireRed5 :: CInt
  , fireBlack :: CInt
  }
  deriving (Show)

newtype FireState
  = FireState [FireStrand]
  deriving (Show)

emptyFire :: Int -> FireState
emptyFire l = FireState $ replicate l emptyFireStrand

emptyFireStrand :: FireStrand
emptyFireStrand = FireStrand
  { fireWhite = 0
  , fireRed1 = 0
  , fireRed2 = 0
  , fireRed3 = 0
  , fireRed4 = 0
  , fireRed5 = 0
  , fireBlack = 0
  }

strandComponents :: Int
strandComponents = 7

maxFireHeight :: CInt
maxFireHeight = 2 + 5 * 5 + 10

pixelSize :: CInt
pixelSize = 10

black, white, red1, red2, red3, red4, red5 :: V4 Word8
black = V4 0 0 0 255
white = V4 255 255 255 255
red1 = V4 255 255 0 255
red2 = V4 230 30 0 255
red3 = V4 190 0 0 255
red4 = V4 130 0 0 255
red5 = V4 50 0 0 255

originY :: CInt
originY = 0

originX :: CInt
originX = 0

fireWidth :: Int
fireWidth = 100

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  ys : chunks n zs
  where
    (ys, zs) = splitAt n xs

updateFire :: [CInt] -> FireState -> FireState
updateFire rnds (FireState ss) =
  FireState $ zipWith updateFireStrand (chunks strandComponents rnds) ss

updateFireStrand :: [CInt] -> FireStrand -> FireStrand
updateFireStrand rnds FireStrand{..} =
  FireStrand
  { fireWhite = rndLength (rnds !! 0) (1, 2)
  , fireRed1 = rndLength (rnds !! 1) (2, 5)
  , fireRed2 = rndLength (rnds !! 2) (2, 5)
  , fireRed3 = rndLength (rnds !! 3) (1, 5)
  , fireRed4 = rndLength (rnds !! 4) (1, 5)
  , fireRed5 = rndLength (rnds !! 5) (2, 5)
  , fireBlack = rndLength (rnds !! 6) (4, 10)
  }

rndLength :: CInt -> (CInt, CInt) -> CInt
rndLength rnd (lo, hi) = rnd `mod` (hi - lo) + 1

renderFire :: Renderer -> FireState -> IO ()
renderFire renderer (FireState ss) =
  mapM_ (uncurry $ renderFireStrand renderer) $ zip [0..] ss

renderFireStrand :: Renderer -> CInt -> FireStrand -> IO ()
renderFireStrand renderer xOffset FireStrand{..} = do
  let yOffset0 = maxFireHeight - fireWhite
      yOffset1 = yOffset0 - fireRed1
      yOffset2 = yOffset1 - fireRed2
      yOffset3 = yOffset2 - fireRed3
      yOffset4 = yOffset3 - fireRed4
      yOffset5 = yOffset4 - originY

  drawPixels fireWhite xOffset yOffset0 white
  drawPixels fireRed1 xOffset yOffset1 red1
  drawPixels fireRed2 xOffset yOffset2 red2
  drawPixels fireRed3 xOffset yOffset3 red3
  drawPixels fireRed4 xOffset yOffset4 red4
  drawPixels fireBlack xOffset yOffset5 black

  where
    drawPixels h left top colour = do
      rendererDrawColor renderer $= colour
      fillRect renderer (Just (Rectangle topLeft widthHeight))
      where
        topLeft = P (V2 (originX + (left * pixelSize)) (originY + (top * pixelSize)))
        widthHeight = V2 pixelSize (pixelSize * h)

eventIsKeyPressed :: Keycode -> Event -> Bool
eventIsKeyPressed keyCode event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed && keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
    _ -> False

fireLoop :: StdGen -> Renderer -> FireState -> IO ()
fireLoop gen renderer fire = do
  rendererDrawColor renderer $= black
  clear renderer

  renderFire renderer fire
  present renderer
  threadDelay (41666 * 3)

  events <- pollEvents
  unless (any (eventIsKeyPressed KeycodeQ) events) $ do
    rnds <- mapM (\_ -> randomIO :: IO CInt) [0..strandComponents * fireWidth]
    let fire' = updateFire rnds fire

    fireLoop gen renderer fire'

main :: IO ()
main = do
  initializeAll
  window <- createWindow "DOOM fire" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  stdGen <- getStdGen

  fireLoop stdGen renderer (emptyFire fireWidth)
