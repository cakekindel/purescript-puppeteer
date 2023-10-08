module Puppeteer.CDPSession where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Puppeteer.Base (CDPSession)

foreign import _detach :: CDPSession -> Effect (Promise Unit)

detach :: CDPSession -> Aff Unit
detach = Promise.toAffE <<< _detach
