module Palette (
  module RandomColor
  ) where

import Effect (Effect)
import Prelude

import Palette.RandomColor as RandomColor
import Palette.Types

main :: Effect Unit
main = do
  -- @TODO
  -- run state monad to test function
  h <- do
     pure $ RandomColor.randomHue HueBlue
  pure unit
