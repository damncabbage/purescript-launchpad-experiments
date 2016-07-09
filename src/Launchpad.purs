module Launchpad
  ( module Data.Maybe
  , module Prelude
  , LAUNCHPAD
  , LaunchEff
  , Connection
  , Configuration
  , Hue(..)
  , Intensity(..)
  , Color(..)
  , Button(..)
  , ButtonColor
  , connect
  , disconnect
  , setButtonColour
  , demo
  , demo'
  ) where

import Prelude
import Debug.Trace (spy)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn4, runFn4)
import Control.Monad.Aff (Aff, makeAff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (log)

foreign import data LAUNCHPAD :: !

foreign import data Connection :: *

type Configuration = { port :: Int }

--withConnection :: forall e. Configuration -> Aff (launchpad :: LAUNCHPAD | e) Unit -> Aff e Unit

type LaunchEff e = (launchpad :: LAUNCHPAD | e)

foreign import connectImpl
  :: forall e
   . (Connection -> Eff (LaunchEff e) Unit)
  -> Configuration
  -> Eff (LaunchEff e) Unit

connect :: forall e. Configuration -> Aff (LaunchEff e) Connection
connect config = makeAff (\_ res -> connectImpl res config)
--connect config = spy $ makeAff (\_ res -> spy $ connectImpl (spy res) (spy config))

foreign import disconnect
  :: forall e. Connection
  -> Eff (LaunchEff e) Unit

foreign import sendMessageImpl
  :: forall e
   . Fn4 Connection Int Int Int (Eff (LaunchEff e) Unit)

data Hue = Red | Yellow | Orange | Green
data Intensity = Low | Medium | High
data Color = Color Hue Intensity
type ButtonColor = Maybe Color

newtype Button = Button { x :: Int, y :: Int }
data SpecialButton =
    TopSpecial Int
  | RightSpecial Int

setButtonColour
  :: forall e
   . Connection
  -> Button
  -> ButtonColor
  -> Eff (LaunchEff e) Unit
setButtonColour conn but col =
  runFn4 sendMessageImpl conn 144 (toNote but) (colorToCode col)
  where
    toNote (Button p) = (p.y * 16) + p.x


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

demo = launchAff do
  c <- connect { port: 0 }
  liftEff $ setButtonColour c (Button { x: 1, y: 1 }) (Just (Color Green High))
  liftEff $ setButtonColour c (Button { x: 2, y: 2 }) (Just (Color Yellow High))
  liftEff $ log "here"
  --liftEff $ disconnect c
  pure unit

demo' = log "...first" >>= \_ -> demo >>= \_ -> log "done"

--setSpecialButtonColour
-- (Uses 176 for the top-row, and just adds one to the RHS I think.)

--boardRegion :: Button -> Button -> Array Button


-- foreign import onPush
