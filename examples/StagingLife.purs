module Examples.StagingLife where

import Debug.Trace (traceAny, spy)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadZero (guard)
import Data.Foldable (elem, any)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Array as Array
import Data.Array.Partial as UnsafeA
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Int as Int
import Data.StrMap as StrMap
import Launchpad -- (LAUNCHPAD, LaunchEff, Connection, Configuration, Hue(..), Intensity(..), Color(..), ButtonColor, ButtonRef, ButtonPress, ButtonPressState, Grid(..), anyButtonPressed, buttonRef, clearAll, connect, disconnect, gridSideLength, gridToButtons, minIndex, maxIndex, mapToGrid, setAll, setGrid, setButtonColor, unsafeButtonRef, unButtonRef, unGrid)
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Signal
import Signal.DOM
import Signal.Time
import X.Array (mapWithIndex)

type Cell = (Tuple Int Int)

-- TODO: justDied, an array cleared every time, that paints just-died cells in Color Red Low
type State =
  { alive :: Array Cell
  , staging ::
      { countdown :: Int
      , cells :: Array Cell
      }
  }

-- TODO: Presets
initialState =
  { alive: [] <> glider (at 0 0)
  , staging:
      { countdown: 0
      , cells: []
      }
  }

glider :: Cell -> Array Cell
glider cell =
  relativeTo cell [at 1 0, at 2 1, at 0 2, at 1 2, at 2 2]

main :: Eff _ Unit
main = do
  c <- connect { port: 0 }
  clearAll c
  buttonPress <- anyButtonPressed c
  let intervalTimer = every 200.0
      looper = foldp nextGameState initialState (sampleOn intervalTimer buttonPress)
  runSignal $ looper ~> renderBoard c

-- More complicated "only put the user's choices in the set of cells
-- after a timeout period has expired" logic, to stop cells being immediately
-- brought into the game and then dying a no-neighbours death.
nextGameState :: Maybe ButtonPress -> State -> State
nextGameState maybePress st =
  let freshPress =
        flip (maybe []) maybePress $ \bp ->
          Array.singleton <<< (\p -> at p.x p.y) <<< unButtonRef $ bp.button.ref
      zeroHour = st.staging.countdown == 0
      advanced = advance st.alive
      newCountDown = 3
      didPress = Array.length freshPress > 0
  in st
      { alive = advanced <>
          if zeroHour
            then st.staging.cells
            else []
      , staging =
          { countdown:
              if didPress
                then newCountDown
                else st.staging.countdown - 1
          , cells:
              if zeroHour
                then []
                else st.staging.cells <> freshPress
          }
      }

cellsToButtons :: Array Cell -> Array Cell -> Array Cell -> Array (Tuple ButtonRef ButtonColor)
cellsToButtons lifeCells stagingCells diedCells =
  let filledLife =
        flip map lifeCells $ \(Tuple x y) ->
          unsafeButtonRef x y /\ Just (Color Green High)
      filledStaging =
        flip map stagingCells $ \(Tuple x y) ->
          unsafeButtonRef x y /\ Just (Color Yellow Medium)
      filledDied =
        flip map diedCells $ \(Tuple x y) ->
          unsafeButtonRef x y /\ Just (Color Red Low)
      blank = do
        x <- Array.range minIndex maxIndex
        y <- Array.range minIndex maxIndex
        guard (not $ any (elem (x /\ y)) [lifeCells, stagingCells, diedCells])
        pure (unsafeButtonRef x y /\ Nothing) -- Button is turned off at this coordinate.
   in Array.concat [filledLife, filledStaging, filledDied, blank]

renderBoard :: Connection -> State -> Eff _ Unit
renderBoard c st =
  setButtons c $ cellsToButtons st.alive st.staging.cells []

advance :: Array Cell -> Array Cell
advance cells = do
  freq <- frequencies $ Array.concatMap neighbourRefs cells
  let cell = fst freq
      n = snd freq
  guard (n == 3 || (n == 2 && cell `elem` cells))
  pure cell

frequencies :: forall a. (Ord a) => Array a -> Array (Tuple a Int)
frequencies xs = do
  x <- Array.group <<< Array.sort $ xs
  pure $ Tuple
    (unsafePartial $ UnsafeA.head x)
    (Array.length x)

neighbourRefs :: Cell -> Array Cell
neighbourRefs (Tuple x y) = do
  dx <- Array.range (-1) 1
  dy <- Array.range (-1) 1
  guard (dx /= 0 || dy /= 0) -- Everything except the centre.
  let xdx = x + dx
      ydy = y + dy
  pure $ Tuple (wrap xdx) (wrap ydy)
  where
    wrap =
      (\v -> if v > maxIndex then minIndex else v) <<<
      (\v -> if v < minIndex then maxIndex else v)


-- Game / Preset Helpers
at :: Int -> Int -> Cell
at = (/\)

relativeTo :: Cell -> Array Cell -> Array Cell
relativeTo (Tuple x y) =
  map (\(Tuple px py) -> (px + x) /\ (py + y))
