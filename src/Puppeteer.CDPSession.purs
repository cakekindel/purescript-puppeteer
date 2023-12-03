module Puppeteer.CDPSession where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Puppeteer.Base (CDPSession)
import Puppeteer.FFI as FFI

foreign import _detach :: CDPSession -> Effect (Promise Unit)

detach :: CDPSession -> Aff Unit
detach = FFI.promiseToAff <<< _detach
