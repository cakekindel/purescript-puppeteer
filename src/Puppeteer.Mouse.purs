module Puppeteer.Mouse
  ( MouseButton(..)
  , scroll
  , down
  , up
  , moveTo
  , click
  , MouseWheelOptions
  , MouseMoveOptions
  , MouseOptions
  , MouseClickOptions
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Prim.Row (class Nub, class Union)
import Puppeteer.Base (Mouse)
import Record (merge, modify) as Record
import Type.Prelude (Proxy(..))

data MouseButton
  = MouseLeft
  | MouseRight
  | MouseMiddle
  | MouseBack
  | MouseForward

mouseButtonToString :: MouseButton -> String
mouseButtonToString MouseLeft = "left"
mouseButtonToString MouseRight = "right"
mouseButtonToString MouseMiddle = "middle"
mouseButtonToString MouseBack = "back"
mouseButtonToString MouseForward = "forward"

type MouseWheelOptions r = (deltaX :: Number, deltaY :: Number | r)
type MouseMoveOptions r = (steps :: Number | r)
type MouseOptions r = (button :: MouseButton | r)
type MouseClickOptions r = (count :: Int, delay :: Number | MouseOptions r)

foreign import scrollImpl :: Mouse -> {deltaX :: Number, deltaY :: Number} -> Effect (Promise Unit)
foreign import clickImpl :: Mouse -> {x :: Number, y :: Number} -> {button :: String, count :: Int, delay :: Number} ->  Effect (Promise Unit)
foreign import downImpl :: Mouse -> String ->  Effect (Promise Unit)
foreign import upImpl :: Mouse -> String ->  Effect (Promise Unit)
foreign import moveImpl :: Mouse -> {x :: Number, y :: Number} -> {steps :: Number} -> Effect (Promise Unit)

scroll :: forall options missing. Union options missing (MouseWheelOptions ()) => Union options (MouseWheelOptions ()) (MouseWheelOptions ()) => Record options -> Mouse -> Aff Unit
scroll options mouse = Promise.toAffE
                     $ scrollImpl mouse
                     $ Record.merge options {deltaX: 0.0, deltaY: 0.0}

down :: MouseButton -> Mouse -> Aff Unit
down btn mouse = Promise.toAffE $ downImpl mouse (mouseButtonToString btn)

up :: MouseButton -> Mouse -> Aff Unit
up btn mouse = Promise.toAffE $ upImpl mouse (mouseButtonToString btn)

moveTo :: forall options missing fullU. Nub fullU (MouseMoveOptions ())
       => Union options missing (MouseMoveOptions ())
       => Union options (MouseMoveOptions ()) fullU
       => Record options
       -> Mouse
       -> {x :: Number, y :: Number}
       -> Aff Unit
moveTo opts mouse xy = Promise.toAffE
                     $ moveImpl mouse xy
                     $ Record.merge opts {steps: 1.0}

click :: forall options missing fullU. Nub fullU (MouseClickOptions ())
      => Union options missing (MouseClickOptions ())
      => Union options (MouseClickOptions ()) fullU
      => Record options
      -> Mouse
      -> {x :: Number, y :: Number}
      -> Aff Unit
click opts mouse xy = Promise.toAffE
                    $ clickImpl mouse xy
                    $ Record.modify (Proxy @"button") mouseButtonToString
                    $ Record.merge opts {button: MouseLeft, count: 1, delay: 0.0}
