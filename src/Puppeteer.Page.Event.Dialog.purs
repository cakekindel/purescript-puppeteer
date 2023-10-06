module Puppeteer.Page.Event.Dialog
  ( Dialog
  , DialogType(..)
  , accept
  , defaultValue
  , dismiss
  , message
  , dialogType
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeFromForeign)
import Puppeteer.FFI as FFI
import Simple.JSON (class ReadForeign)

data DialogType
  = Alert
  | Confirm
  | Prompt
  | BeforeUnload

dialogTypeOfString :: String -> DialogType
dialogTypeOfString "confirm" = Confirm
dialogTypeOfString "prompt" = Prompt
dialogTypeOfString "beforeunload" = BeforeUnload
dialogTypeOfString _ = Alert

dialogTypeString :: DialogType -> String
dialogTypeString Alert = "alert"
dialogTypeString Confirm = "confirm"
dialogTypeString Prompt = "prompt"
dialogTypeString BeforeUnload = "beforeunload"

foreign import data Dialog :: Type

instance dialogForeign :: ReadForeign Dialog where
  readImpl = pure <<< unsafeFromForeign

foreign import defaultValue :: Dialog -> String
foreign import message :: Dialog -> String
foreign import _dismiss :: Dialog -> Effect (Promise Unit)
foreign import _accept :: Foreign -> Dialog -> Effect (Promise Unit)
foreign import _type :: Dialog -> String

dismiss :: Dialog -> Aff Unit
dismiss = Promise.toAffE <<< _dismiss

accept :: Maybe String -> Dialog -> Aff Unit
accept s = Promise.toAffE <<< _accept (FFI.maybeToUndefined s)

dialogType :: Dialog -> DialogType
dialogType = dialogTypeOfString <<< _type
