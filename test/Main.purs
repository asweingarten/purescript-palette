module Test.Main where

import Prelude
import Control.Monad.State (runState)
import Data.Tuple as T
import Palette.RandomColor
import Effect (Effect)
import Effect.Console (log)
import Random.LCG (mkSeed)

import Palette.RandomColor as RandomColor
import Palette.Types

main :: Effect Unit
main = do
  log "You should add some tests."
  randomHueTest
  randomColorTest
  saturationRangeTest


-- @TODO: write test for randomColor
randomHueTest :: Effect Unit
randomHueTest = do
  log "randomHueTest"
  let (T.Tuple hue seed') = flip runState (mkSeed 1) $  RandomColor.randomHue HueBlue
  log $ show hue
  pure unit

randomColorTest :: Effect Unit
randomColorTest = do
  log "randomColorTest"
  let (T.Tuple color seed') = flip runState (mkSeed 1) $ RandomColor.randomColor HueBlue LumLight
  log $ show color
  pure unit

saturationRangeTest :: Effect Unit
saturationRangeTest = do
  let x = RandomColor.saturationRange HueBlue
  log $ show x

