module Puppeteer.Browser
  ( module X
  , Product(..)
  , ChromeReleaseChannel(..)
  , Connect
  , disconnect
  , websocketEndpoint
  , connected
  , prepareConnectOptions
  , get
  , close
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Enum (fromEnum)
import Data.Maybe (Maybe)
import Data.Time (Millisecond)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Puppeteer.Base (Browser) as X
import Puppeteer.Base (class BrowserAccess, Browser, BrowserContext, Viewport)
import Puppeteer.FFI as FFI
import Simple.JSON (writeImpl)

data Product
  = Chrome
  | Firefox

data ChromeReleaseChannel
  = ChromeStable
  | ChromeBeta
  | ChromeCanary
  | ChromeDev

type Connect =
  { defaultViewport :: Maybe Viewport
  , ignoreHTTPSErrors :: Maybe Boolean
  , protocolTimeout :: Maybe Millisecond
  , slowMo :: Maybe Millisecond
  }

prepareViewport :: Viewport -> Foreign
prepareViewport
  { deviceScaleFactor
  , hasTouch
  , height
  , width
  , isLandscape
  , isMobile
  } = writeImpl
  { deviceScaleFactor: FFI.maybeToUndefined deviceScaleFactor
  , hasTouch: FFI.maybeToUndefined hasTouch
  , height
  , width
  , isLandscape: FFI.maybeToUndefined isLandscape
  , isMobile: FFI.maybeToUndefined isMobile
  }

prepareConnectOptions :: Connect -> Foreign
prepareConnectOptions
  { defaultViewport
  , ignoreHTTPSErrors
  , protocolTimeout
  , slowMo
  } = writeImpl
  { defaultViewport: FFI.maybeToUndefined $ map prepareViewport defaultViewport
  , ignoreHTTPSErrors: FFI.maybeToUndefined ignoreHTTPSErrors
  , protocolTimeout: FFI.maybeToUndefined $ map fromEnum protocolTimeout
  , slowMo: FFI.maybeToUndefined $ map fromEnum slowMo
  }

foreign import _close :: Browser -> Promise Unit
foreign import _get :: Foreign -> Effect Browser

foreign import disconnect :: Browser -> Effect Unit
foreign import websocketEndpoint :: Browser -> Effect String
foreign import connected :: Browser -> Effect Boolean
foreign import ofContext :: BrowserContext -> Effect Browser

get :: forall b. BrowserAccess b => b -> Effect Browser
get = _get <<< unsafeToForeign

close :: Browser -> Aff Unit
close = Promise.toAff <<< _close
