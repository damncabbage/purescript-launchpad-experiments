module Examples.AllPad where

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

type State = { currentColor :: ButtonColor }

initialState = { currentColor: Nothing }

main = do
  c <- connect { port: 0 }
  clearAll c
  let intervalTimer = every 140.0
      looper = foldp loopLogic initialState intervalTimer
  runSignal $ looper ~> renderBoard c

loopLogic :: _ -> State -> State
loopLogic _ s =
  let nextColor =
        case s.currentColor of
          Nothing -> Just $ Color Red High
          Just (Color Red _) -> Just $ Color Orange High
          Just (Color Orange _) -> Just $ Color Yellow High
          Just (Color Yellow _) -> Just $ Color Green High
          Just (Color Green _) -> Just $ Color Red Low
   in s { currentColor = nextColor }

renderBoard :: Connection -> State -> Eff _ Unit
renderBoard c s =
  setAll c s.currentColor
