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

type State =
  { currentColor :: ButtonColor
  , rotNum :: Int
  , pressed :: StrMap.StrMap (Tuple ButtonRef ButtonColor)
  }

initialState =
  { currentColor: Nothing
  , rotNum: 0
  , pressed: StrMap.fromFoldable []
  }

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
  clearAll c
  buttonPress <- anyButtonPressed c
  let intervalTimer = every 60.0
      looper = foldp addPressedToState initialState (sampleOn intervalTimer buttonPress)
  -- runSignal $ looper ~> renderBoard c
  --let looper = foldp loopLogic initialState (sampleOn (every 60.0) {})
  runSignal (renderPressedBoard c <$> looper)

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
