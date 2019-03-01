module Main where

import Color (Color)
import Data.Maybe
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
  | n' == 0   = HueMonochrome
  | n' >= 283 = HuePink
  | n' >= 258 = HuePurple
  | n' >= 179 = HueBlue
  | n' >= 63  = HueGreen
  | n' >= 47  = HueYellow
  | n' >= 19  = HueOrange
  | n' >= -26 = HueRed
  | otherwise = error "getHue: hue outside [0, 360]"
  where
    n' = if n >= 334 && n <= 360 then n - 360 else n

main :: Effect Unit
main = do
  log "Hello sailor!"
