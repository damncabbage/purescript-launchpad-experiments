module Examples.ButtonPad where

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
import Launchpad (LAUNCHPAD, LaunchEff, Connection, Configuration, Hue(..), Intensity(..), Color(..), ButtonColor, ButtonRef, ButtonPress, ButtonPressState, Grid(..), anyButtonPressed, buttonRef, clearAll, connect, disconnect, gridSideLength, gridToButtons, minIndex, maxIndex, mapToGrid, setAll, setGrid, setButtons, setButtonColor, unsafeButtonRef, unButtonRef, unGrid)
import Signal
import Signal.DOM
import Signal.Time
import X.Array (mapWithIndex)

type State =
  { pressed :: StrMap.StrMap (Tuple ButtonRef ButtonColor) }

initialState =
  { pressed: StrMap.fromFoldable [] }

main = do
  c <- connect { port: 0 }
  clearAll c
  buttonPress <- anyButtonPressed c
  let intervalTimer = every 60.0
      looper = foldp addPressedToState initialState (sampleOn intervalTimer buttonPress)
  runSignal $ looper ~> renderPressedBoard c

addPressedToState :: ButtonPress -> State -> State
addPressedToState bp st =
  let col = if bp.deltaTime < 0.5
              then Color Yellow High
              else if bp.deltaTime < 1.0
                then Color Orange High
                else Color Red High
   in case bp.button of
        Just b ->
          traceAny (b.ref) $ \_ ->
            unButtonRef b.ref # \r ->
              st { pressed =
                StrMap.insert
                  (show r.x <> "," <> show r.y)
                  (b.ref /\ Just col)
                  st.pressed
              }
        Nothing ->
          st

renderPressedBoard :: Connection -> State -> Eff _ Unit
renderPressedBoard c st =
  setButtons c $ Array.fromFoldable st.pressed
