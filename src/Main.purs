module Main where

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
import Launchpad -- (LAUNCHPAD, LaunchEff, Connection, Configuration, Hue(..), Intensity(..), Color(..), ButtonColor, ButtonRef, ButtonPress, ButtonPressState, Grid(..), anyButtonPressed, buttonRef, clearAll, connect, disconnect, gridSideLength, gridToButtons, minIndex, maxIndex, mapToGrid, setAll, setGrid, setButtonColor, unsafeButtonRef, unButtonRef, unGrid)
import Signal
import Signal.DOM
import Signal.Time
import X.Array (mapWithIndex)

data Cell = Alive | Dead

type State =
  { grid :: Array (Array Cell) }

initialState =
  { grid: [] }

main = do
  c <- connect { port: 0 }
  clearAll c
  buttonPress <- anyButtonPressed c
  let intervalTimer = every 60.0
      looper = foldp nextGameState initialState (sampleOn intervalTimer buttonPress)
  runSignal $ looper ~> renderBoard c

nextGameState :: Maybe ButtonPress -> State -> State
nextGameState maybePress st =
  let userContribution =
        case maybePress of
          Nothing -> []
          Just bp -> []
   in st -- TODO

cellsToButtons :: Array (Array Cell) -> Array (Tuple ButtonRef ButtonColor)
cellsToButtons xs = [] -- TODO

renderBoard :: Connection -> State -> Eff _ Unit
renderBoard c st =
  setButtons c <<< cellsToButtons <<< Array.fromFoldable $ st.grid

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
