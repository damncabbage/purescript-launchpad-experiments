module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Int as Int
import Math ((%))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Launchpad (LAUNCHPAD, LaunchEff, Connection, Configuration, Hue(..), Intensity(..), Color(..), ButtonColor, Grid(..), buttonRef, connect, disconnect, gridSideLength, gridToButtons, minIndex, maxIndex, mapToGrid, setAll, setGrid, setButtonColor, unsafeButtonRef)
import Signal
import Signal.DOM
import Signal.Time

type State =
  { currentColor :: ButtonColor
  , rotNum :: Int
  }

initialState =
  { currentColor: Nothing
  , rotNum: 0
  }

sineGrid =
  map
    (\_ -> Array.range minIndex maxIndex)
    (Array.range minIndex maxIndex)

crummySineGrid rotNum =
  let sine = crummySineGridArrays
      len = Array.length sine
   in mapToGrid $ flip map sine $ \s ->
        (Array.slice rotNum len s) <>
        (Array.slice 0 rotNum s)

crummySineGridArrays =
  flip mapWithIndex strings $ \(idx /\ row) ->
    mapWithIndex
      (\(idx /\ c) -> charToCol c)
      (String.toCharArray row)
  where
    mapWithIndex :: forall a b. ((Tuple Int a) -> b) -> Array a -> Array b
    mapWithIndex f = map f <<< Array.zip (Array.range 0 7)
    charToCol c =
      case c of
        'G' -> Just (Color Green High)
        'R' -> Just (Color Red High)
        'Y' -> Just (Color Yellow High)
        _ -> Nothing
    strings =
      ["GG      G"
      ,"RRG    GR"
      ,"YYRG  GRY"
      ,"  RG  GR "
      ,"  YRGGRY "
      ,"   RGGR  "
      ,"   YRRY  " 
      ,"    YY   "]




main = do
  c <- connect { port: 0 }
  let looper = foldp loopLogic initialState (every 60.0)
  -- runSignal $ looper ~> renderBoard c
  runSignal (renderBoard c <$> looper)

loopLogic :: _ -> State -> State
loopLogic _ s =
  let nextColor =
        case s.currentColor of
          Nothing -> Just $ Color Red High
          Just (Color Red _) -> Just $ Color Orange High
          Just (Color Orange _) -> Just $ Color Yellow High
          Just (Color Yellow _) -> Just $ Color Green High
          Just (Color Green _) -> Just $ Color Red Low
      nextNum = s.rotNum + 1
   in s { currentColor = nextColor
        , rotNum = if nextNum > 7 then 0 else nextNum
        }

renderBoard :: Connection -> State -> Eff _ Unit
renderBoard c s =
  --setAll c s.currentColor
  setGrid c <<< gridToButtons $ crummySineGrid s.rotNum
