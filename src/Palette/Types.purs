module Palette.Types where

import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnumWithDefaults)
import Data.Generic.Rep as G
import Data.Generic.Rep.Enum as GEnum
import Data.Generic.Rep.Bounded as GBounded
import Data.List (List)
import Data.Maybe(Maybe)
import Data.Pair (Pair(..))
import Prelude
import Random.PseudoRandom (class Random, randomR, Seed)

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

derive instance genericHue :: G.Generic Hue _
derive instance eqHue :: Eq Hue
derive instance ordHue :: Ord Hue
instance boundedHue :: Bounded Hue where
  bottom = GBounded.genericBottom
  top    = GBounded.genericTop
instance enumHue :: Enum Hue where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumHue :: BoundedEnum Hue where
  cardinality = GEnum.genericCardinality
  toEnum      = GEnum.genericToEnum
  fromEnum    = GEnum.genericFromEnum

instance randomHue' :: Random Hue where
  random seed = { newVal: newVal, newSeed: intRp.newSeed }
    where
      intRp = (randomR (fromEnum (bottom::Hue)) (fromEnum (top::Hue)) seed)
      newVal = toEnumWithDefaults bottom top intRp.newVal
  randomR min max seed
    | min > max = randomR max min seed
    | otherwise = { newVal: newVal, newSeed: intRp.newSeed }
      where
        intRp = randomR (fromEnum min) (fromEnum max) seed
        newVal = toEnumWithDefaults bottom top intRp.newVal

-- | Used to select the luminosity range in 'randomColor'.
data Luminosity
  = LumBright
  | LumLight
  | LumDark
  | LumRandom
  {-- deriving (Show, Eq) --}

type ColorDefinition = { hueRange    :: Maybe (Pair Int)
                       , lowerBounds :: List (Pair Int)
                       }
