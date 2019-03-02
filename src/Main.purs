module Main where

import Color (Color)
import Data.List
import Data.Maybe
import Data.Pair
import Prelude
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Console (log)

-- | Used to select the hue range in 'randomColor'.
data Hue
  = HueMonochrome
  | HueRed
  | HueOrange
  | HueYellow
  | HueGreen
  | HueBlue
  | HuePurple
  | HuePink
  | HueRandom
  {-- deriving (Show, Eq) --}
  {-- https://pursuit.purescript.org/packages/purescript-generics-rep/6.1.1/docs/Data.Generic.Rep.Show#v:genericShow --}

-- | Used to select the luminosity range in 'randomColor'.
data Luminosity
  = LumBright
  | LumLight
  | LumDark
  | LumRandom
  {-- deriving (Show, Eq) --}

type ColorDefinition = { hueRange    :: Maybe (Pair Int Int)
                       , lowerBounds :: List (Pair Int Int)
                       }

-- port this function over
getHue :: Int -> Hue
getHue n =
  let n' = if n >= 334 && n <= 360 then n - 360 else n
   in
   case n' of
        0   -> HueMonochrome
        283 -> HuePink
        258 -> HuePurple
        179 -> HueBlue
        63  -> HueGreen
        47  -> HueYellow
        19  -> HueOrange
        -26 -> HueRed
        _   -> unsafeThrow "getHue: hue outside [0, 360]"


main :: Effect Unit
main = do
  log "Hello sailor!"
