module Examples.Wave where

import Debug.Trace (traceAny)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Int as Int
import Data.StrMap as StrMap
import Math ((%))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Launchpad (LAUNCHPAD, LaunchEff, Connection, Configuration, Hue(..), Intensity(..), Color(..), ButtonColor, ButtonRef, ButtonPress, ButtonPressState, Grid(..), anyButtonPressed, buttonRef, clearAll, connect, disconnect, gridSideLength, gridToButtons, minIndex, maxIndex, mapToGrid, setAll, setGrid, setButtonColor, unsafeButtonRef, unButtonRef, unGrid)
import Signal
import Signal.DOM
import Signal.Time
import X.Array (mapWithIndex)

type State = { rotNum :: Int }

initialState = { rotNum: 0 }

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
  clearAll c
  let intervalTimer = every 60.0
      looper = foldp loopLogic initialState intervalTimer
  runSignal $ looper ~> renderBoard c

loopLogic :: _ -> State -> State
loopLogic _ s =
  let nextNum = s.rotNum + 1
   in s {
        rotNum = if nextNum > 7 then 0 else nextNum
      }

renderBoard :: Connection -> State -> Eff _ Unit
renderBoard c s =
  setGrid c <<< gridToButtons $ crummySineGrid s.rotNum
