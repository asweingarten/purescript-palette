module Palette.RandomColor where

import Color (Color, ColorSpace(..), hsv)
import Control.Monad.State (State, get, put)
import Data.Foldable (find)
import Data.Int (round, toNumber)
import Data.List (List(..), fromFoldable, zip, (:), head, last)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Pair (Pair(..), fst)
import Data.Tuple as T
import Debug.Trace (trace)
import Partial.Unsafe (unsafePartial)
import Prelude
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Console (log)
import Random.PseudoRandom (class Random, randomR, Seed)

import Palette.Types

-- @TODO: This function is WRONG. The numbers should be ranges (duh). I'm not sure what the best way to write it is.
-- Probs a good idea to not spend 5 minutes figuring out a cleaner solution than a bunch of ifs
getHue :: Int -> Hue
getHue n =
  case (if n >= 334 && n <= 360 then n - 360 else n) of
       0 -> HueMonochrome
       n' | n' == 0   -> HueMonochrome
          | n' >= 283 -> HuePink
          | n' >= 258 -> HuePurple
          | n' >= 179 -> HueBlue
          | n' >= 63  -> HueGreen
          | n' >= 47  -> HueYellow
          | n' >= 19  -> HueOrange
          | n' >= -26 -> HueRed
          | otherwise -> unsafeThrow "getHue: hue outside [0, 360]"

getColorDefinition :: Hue -> ColorDefinition
getColorDefinition HueMonochrome = { hueRange: Nothing
                                   , lowerBounds: fromFoldable [Pair 0 0, Pair 100 0]
                                   }
getColorDefinition HueRed        = { hueRange: Just (Pair (-26) 18)
                                   , lowerBounds: fromFoldable [ Pair 20 100 , Pair 30 92, Pair 40 89, Pair 50 85
                                                               , Pair 60 78, Pair 70 70 , Pair 80 60, Pair 90 55, Pair 100 50
                                                               ]
                                   }
getColorDefinition HueOrange     = { hueRange: Just (Pair 19 46)
                                   , lowerBounds: fromFoldable [ Pair 20 100, Pair 30 93, Pair 40 88, Pair 50 86, Pair 60 85, Pair 70 70 , Pair 100 70 ]
                                   }
getColorDefinition HueYellow     = { hueRange: Just (Pair 47 62)
                                   , lowerBounds: fromFoldable [ Pair 25 100, Pair 40 94, Pair 50 89, Pair 60 86, Pair 70 84, Pair 80 82 , Pair 90 80, Pair 100 75 ]
                                   }
getColorDefinition HueGreen      = { hueRange: Just (Pair 63 178)
                                   , lowerBounds: fromFoldable [ Pair 30 100, Pair 40 90, Pair 50 85, Pair 60 81, Pair 70 74, Pair 80 64 , Pair 90 50, Pair 100 40 ]
                                   }
getColorDefinition HueBlue       = { hueRange: Just (Pair 179 257)
                                   , lowerBounds: fromFoldable [ Pair 20 100, Pair 30 86, Pair 40 80, Pair 50 74, Pair 60 60, Pair 70 52 , Pair 80 44, Pair 90 39, Pair 100 35 ]
                                   }
getColorDefinition HuePurple     = { hueRange: Just (Pair 258 282)
                                   , lowerBounds: fromFoldable [ Pair 20 100, Pair 30 87, Pair 40 79, Pair 50 70, Pair 60 65, Pair 70 59 , Pair 80 52, Pair 90 45, Pair 100 42 ]
                                   }
getColorDefinition HuePink       = { hueRange: Just (Pair 283 334)
                                   , lowerBounds: fromFoldable [ Pair 20 100, Pair 30 90, Pair 40 86, Pair 60 84, Pair 80 80, Pair 90 75 , Pair 100 73 ]
                                   }
getColorDefinition HueRandom     = { hueRange: Just (Pair 0 359)
                                   , lowerBounds: fromFoldable []
                                   }

randomHue :: Hue -> State Seed Int
randomHue h = do
  seed <- get
  let randomPair = randomR lo hi seed
  let hue = randomPair.newVal
  put randomPair.newSeed
  pure $ if hue < 0 then hue + 360 else hue
  where
    hr = _.hueRange <<< getColorDefinition $ h
    Pair lo hi = fromMaybe (Pair 0 0) hr

saturationRange :: Hue -> Pair Int
saturationRange hue = result
  where
    lbs = _.lowerBounds $ getColorDefinition hue
    result = case lbs of
      Nil -> unsafeThrow "Can\'t obtain saturationRange from an empty lowerBounds"
      -- so jank with fromMaybe default value
      _  -> Pair (fst $ fromMaybe (Pair 0 0) (head $ lbs)) (fst $ fromMaybe (Pair 0 0) (last $ lbs))

randomSaturation :: Hue -> Luminosity -> State Seed Int
randomSaturation HueMonochrome _   = pure 0
randomSaturation hue           lum = do
  seed <- get
  let randomPair = case lum of
                      LumRandom -> randomR 0 100 seed
                      LumBright -> randomR 55 hi seed
                      LumDark   -> randomR (hi - 10) hi seed
                      LumLight  -> randomR lo 55 seed
  put randomPair.newSeed
  pure randomPair.newVal
  where
    Pair lo hi = saturationRange hue

minBrightness :: Hue -> Int -> Int
minBrightness hue saturationValue =  fromMaybe 0 result
  where
    lbs = map (\(Pair x y) -> T.Tuple x y)
          <<< _.lowerBounds
          $ getColorDefinition hue
    tup a  = zip (0:a) a
    inRange j (T.Tuple k n) = j >= k && j <= n
    result :: Maybe Int
    result = do
      T.Tuple s1 s2 <- find (inRange saturationValue) (tup $ map T.fst lbs)
      v1       <- T.lookup s1 lbs
      v2       <- T.lookup s2 lbs
      let m =  (v2 - v1) /  (s2 -s1)
          b =  v1 - m *  s1
      pure $ m *  saturationValue + b

-- | Pick a random brightness value given a 'Hue', 'Luminosity' and saturation.
randomBrightness :: Hue -> Luminosity -> Int -> State Seed Int
randomBrightness hue lum saturationValue = do
  seed <- get
  let randomPair = randomR bMin bMax seed
  put randomPair.newSeed
  pure randomPair.newVal
  where
    b            = minBrightness hue saturationValue
    Pair bMin bMax = case lum of
      LumBright -> Pair b 100
      LumDark   -> Pair b (b + 20)
      LumLight  -> Pair ((b + 100) `div` 2) 100
      LumRandom -> Pair 0 100

-- /Better to use 'randomCIELab' for truly random colors/.
randomColor :: Hue -> Luminosity -> State Seed Color
randomColor hue lum = do
  hueValue <- randomHue hue
  let hue'  = getHue hueValue
  satValue <- randomSaturation hue' lum
  briValue <- randomBrightness hue' lum satValue
  pure $ hsv (toNumber hueValue) ((toNumber satValue) / 100.0) ((toNumber briValue) / 100.0)

-- TODO
-- - generate random colors
-- - color woobles randomly using this library

