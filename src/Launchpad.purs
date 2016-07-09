module Launchpad
  ( LAUNCHPAD
  , ButtonColor
  , ButtonRef
  , ButtonPress
  , ButtonPressState(..)
  , Color(..)
  , Configuration
  , Connection
  , Grid
  , Hue(..)
  , Intensity(..)
  , LaunchEff
  , anyButtonPressed
  , buttonRef
  , clearAll
  , connect
  , demo
  , disconnect
  , gridSideLength
  , gridToButtons
  , minIndex
  , maxIndex
  , mapToGrid
  , setAll
  , setGrid
  , setButtonColor
  , unsafeButtonRef
  , unButtonRef
  , unGrid
  ) where

import Prelude
import Debug.Trace (spy)
import Data.Int as Int
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Foldable (all)
import Data.Traversable (traverse, for_)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Data.Function.Uncurried (Fn4, Fn7, runFn4, runFn7)
import Control.Monad.Aff (Aff, makeAff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (log)
import Global as Global
import Math ((%))
import Signal (Signal, constant, (~>))

foreign import data LAUNCHPAD :: !

foreign import data Connection :: *

type Configuration = { port :: Int }


type LaunchEff e = (launchpad :: LAUNCHPAD | e)

foreign import connect
  :: forall e
   . Configuration
  -> Eff (LaunchEff e) Connection

foreign import disconnect
  :: forall e. Connection
  -> Eff (LaunchEff e) Unit

foreign import sendMessageImpl
  :: forall e
   . Fn4 Connection Int Int Int (Eff (LaunchEff e) Unit)

foreign import anyButtonPressedP :: forall a x y e c. Fn7
  Connection
  (c -> Signal c)
  (a -> Maybe a)
  (Maybe a)
  (y -> (x -> y) -> Maybe x -> y)
  (String -> Maybe ButtonRef)
  (Boolean -> ButtonPressState)
  (Eff (LaunchEff e) (Signal ButtonPress))

type ButtonPress =
  { deltaTime :: Number
  , button ::
      Maybe
        { ref :: ButtonRef
        , pressed :: ButtonPressState
        }
  }
data ButtonPressState =
    ButtonPressed
  | ButtonReleased

anyButtonPressed :: forall e. Connection -> Eff (LaunchEff e) (Signal ButtonPress)
anyButtonPressed conn =
  runFn7 anyButtonPressedP conn constant Just Nothing maybe fromNote $ \b ->
    if b then ButtonPressed else ButtonReleased
  where
    fromNote v =
      let num = Global.readNumber v
          y = Int.floor (num / 16.0)
          x = Int.floor (num % 16.0)
       in buttonRef x y

withConnection
  :: forall e
   . Configuration
  -> (Connection -> Eff (LaunchEff e) Unit)
  -> Eff (LaunchEff e) Unit
withConnection config action = do
  c <- connect config
  action c
  disconnect c

demo = do
  log "Starting..."
  withConnection { port: 0 } $ \c -> do
    clearAll c
    for_ (rectRegion topLeft bottomRight) $ \b -> setButtonColor c b (Just $ Color Red High)
  log "Done."


data Hue = Red | Yellow | Orange | Green
data Intensity = Low | Medium | High
data Color = Color Hue Intensity
type ButtonColor = Maybe Color

newtype ButtonRef = ButtonRef { x :: Int, y :: Int }

unButtonRef :: ButtonRef -> { x :: Int, y :: Int }
unButtonRef (ButtonRef r) = r

data SpecialButtonRef =
    TopSpecial Int
  | RightSpecial Int

setButtonColor
  :: forall e
   . Connection
  -> ButtonRef
  -> ButtonColor
  -> Eff (LaunchEff e) Unit
setButtonColor conn but col =
  runFn4 sendMessageImpl conn 144 (toNote but) (colorToCode col)
  where
    toNote (ButtonRef p) = (p.y * 16) + p.x

colorToCode :: ButtonColor -> Int
colorToCode Nothing = 0
colorToCode (Just (Color hue intens)) =
  case hue of
    Red ->
      case intens of
        Low -> 1
        Medium -> 2
        High -> 3
    Yellow ->
      case intens of
        Low -> 17
        Medium -> 34
        High -> 54
    Orange ->
      case intens of
        Low -> 45
        Medium -> 46
        High -> 23
    Green ->
      case intens of
        Low -> 16
        Medium -> 32
        High -> 48

--setSpecialButtonColour
-- (Uses 176 for the top-row, and just adds one to the RHS I think.)

rectRegion :: ButtonRef -> ButtonRef -> Array ButtonRef
rectRegion (ButtonRef from) (ButtonRef to) = do
  x <- Array.range from.x to.x
  y <- Array.range from.y to.y
  pure $ unsafeButtonRef x y

clearAll :: forall e. Connection -> Eff (LaunchEff e) Unit 
clearAll c =
  for_ (rectRegion topLeft bottomRight) $ \b ->
    setButtonColor c b Nothing
             
setAll :: forall e. Connection -> ButtonColor -> Eff (LaunchEff e) Unit
setAll conn col =
  for_ (rectRegion topLeft bottomRight) $ \b ->
    setButtonColor conn b col

setGrid :: forall e. Connection -> Grid (Tuple ButtonRef ButtonColor) -> Eff (LaunchEff e) Unit
setGrid conn (Grid gs) =
  for_ gs $ \ggs ->
    for_ ggs $ \(br /\ bc) ->
      setButtonColor conn br bc --(?uncurry bb)

gridToButtons :: Grid ButtonColor -> Grid (Tuple ButtonRef ButtonColor)
gridToButtons (Grid gs) =
  Grid $
    flip mapWithIndex gs $ \(yi /\ row) ->
      flip mapWithIndex row $ \(xi /\ col) ->
        (unsafeButtonRef xi yi /\ col)

mapWithIndex :: forall a b. ((Tuple Int a) -> b) -> Array a -> Array b
mapWithIndex f = map f <<< Array.zip (Array.range 0 7)

{-
setAllForGrid :: forall e. Connection -> Grid ButtonColor -> Eff (LaunchEff e) Unit
setAllForGrid conn (Grid gs) =
  for_  $ \b ->
    setButtonColor conn b col
    -}

topLeft :: ButtonRef
topLeft = unsafeButtonRef 0 0

bottomRight :: ButtonRef
bottomRight = unsafeButtonRef 7 7

buttonRef :: Int -> Int -> Maybe ButtonRef
buttonRef x y =
  if x >= 0 && x <= 7 && y >= 0 && y <= 7 
    then Just $ unsafeButtonRef x y
    else Nothing

unsafeButtonRef :: Int -> Int -> ButtonRef
unsafeButtonRef x y = ButtonRef {x,y}

data Grid a = Grid (Array (Array a))
-- Maps as if entirely flat.
instance gridFunctor :: Functor Grid where
  map f (Grid ys) = Grid $ map (map f) ys

unGrid :: forall a. Grid a -> (Array (Array a))
unGrid (Grid xs) = xs

mapToGrid
  :: forall a
   . Array (Array a)
  -> Grid a
mapToGrid xs =
  if Array.length xs == len && all (\xs -> Array.length xs == len) xs
    then Grid xs -- Shortcut
    else Grid $ chopToSize len (map (chopToSize len) xs)
  where
    len = gridSideLength

chopToSize :: forall a. Int -> Array a -> Array a
chopToSize maxLen arr =
  chopEvery (intRem (Array.length arr) maxLen) arr
  where
    intRem a b = Int.floor $ (Int.toNumber a) % (Int.toNumber b)
    chopEvery n arr =
      arr -- TODO



minIndex :: Int
minIndex = 0

maxIndex :: Int
maxIndex = gridSideLength - 1

gridSideLength :: Int
gridSideLength = 8

-- foreign import onPush
