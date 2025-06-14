module Puppeteer.Mouse
  ( MouseButton(..)
  , scroll
  , down
  , up
  , moveTo
  , click
  , mouseButtonToString
  , mouseButtonFromString
  , MouseWheelOptions
  , MouseMoveOptions
  , MouseOptions
  , MouseClickOptions
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, upFrom)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff)
import Prim.Row (class Nub, class Union)
import Puppeteer.Base (Mouse)
import Puppeteer.FFI as FFI
import Record (merge, modify) as Record
import Type.Prelude (Proxy(..))

data MouseButton
  = MouseLeft
  | MouseRight
  | MouseMiddle
  | MouseBack
  | MouseForward

derive instance Generic MouseButton _
derive instance Eq MouseButton
derive instance Ord MouseButton
instance Show MouseButton where
  show = genericShow

instance Enum MouseButton where
  pred = genericPred
  succ = genericSucc

instance Bounded MouseButton where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum MouseButton where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

mouseButtonToString :: MouseButton -> String
mouseButtonToString MouseLeft = "left"
mouseButtonToString MouseRight = "right"
mouseButtonToString MouseMiddle = "middle"
mouseButtonToString MouseBack = "back"
mouseButtonToString MouseForward = "forward"

mouseButtonFromString :: String -> Maybe MouseButton
mouseButtonFromString s = find (eq s <<< mouseButtonToString) (upFrom bottom :: Array MouseButton)

type MouseWheelOptions r = (deltaX :: Number, deltaY :: Number | r)
type MouseMoveOptions r = (steps :: Number | r)
type MouseOptions r = (button :: MouseButton | r)
type MouseClickOptions r = (count :: Int, delay :: Number | MouseOptions r)

foreign import scrollImpl :: Mouse -> { deltaX :: Number, deltaY :: Number } -> Effect (Promise Unit)
foreign import clickImpl :: Mouse -> { x :: Number, y :: Number } -> { button :: String, count :: Int, delay :: Number } -> Effect (Promise Unit)
foreign import downImpl :: Mouse -> String -> Effect (Promise Unit)
foreign import upImpl :: Mouse -> String -> Effect (Promise Unit)
foreign import moveImpl :: Mouse -> { x :: Number, y :: Number } -> { steps :: Number } -> Effect (Promise Unit)

scroll :: forall options missing. Union options missing (MouseWheelOptions ()) => Union options (MouseWheelOptions ()) (MouseWheelOptions ()) => Record options -> Mouse -> Aff Unit
scroll options mouse = FFI.promiseToAff
  $ scrollImpl mouse
  $ Record.merge options { deltaX: 0.0, deltaY: 0.0 }

down :: MouseButton -> Mouse -> Aff Unit
down btn mouse = FFI.promiseToAff $ downImpl mouse (mouseButtonToString btn)

up :: MouseButton -> Mouse -> Aff Unit
up btn mouse = FFI.promiseToAff $ upImpl mouse (mouseButtonToString btn)

moveTo
  :: forall options missing fullU
   . Nub fullU (MouseMoveOptions ())
  => Union options missing (MouseMoveOptions ())
  => Union options (MouseMoveOptions ()) fullU
  => Record options
  -> Mouse
  -> { x :: Number, y :: Number }
  -> Aff Unit
moveTo opts mouse xy = FFI.promiseToAff
  $ moveImpl mouse xy
  $ Record.merge opts { steps: 1.0 }

click
  :: forall options missing fullU
   . Nub fullU (MouseClickOptions ())
  => Union options missing (MouseClickOptions ())
  => Union options (MouseClickOptions ()) fullU
  => Record options
  -> Mouse
  -> { x :: Number, y :: Number }
  -> Aff Unit
click opts mouse xy = FFI.promiseToAff
  $ clickImpl mouse xy
  $ Record.modify (Proxy @"button") mouseButtonToString
  $ Record.merge opts { button: MouseLeft, count: 1, delay: 0.0 }
