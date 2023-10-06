module Puppeteer.Browser
  ( module X
  , Product(..)
  , ChromeReleaseChannel(..)
  , Connect
  , duplexConnect
  , duplexProduct
  , duplexChromeReleaseChannel
  , disconnect
  , websocketEndpoint
  , connected
  , get
  , close
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Puppeteer.Base (Browser) as X
import Puppeteer.Base (class BrowserAccess, Browser, BrowserContext, JsDuplex(..), Viewport, duplex)
import Puppeteer.FFI as FFI
import Record (modify)
import Simple.JSON (writeImpl)
import Type.Prelude (Proxy(..))

data Product
  = Chrome
  | Firefox

derive instance Generic Product _
derive instance Eq Product
instance Show Product where
  show = genericShow

duplexProduct :: JsDuplex Product String
duplexProduct =
  let
    toString Chrome = "chrome"
    toString Firefox = "firefox"
    fromString "chrome" = pure Chrome
    fromString "firefox" = pure Firefox
    fromString o = Left $ "unknown browser product " <> o
  in
    duplex toString fromString

data ChromeReleaseChannel
  = ChromeStable
  | ChromeBeta
  | ChromeCanary
  | ChromeDev

derive instance Generic ChromeReleaseChannel _
derive instance Eq ChromeReleaseChannel
instance Show ChromeReleaseChannel where
  show = genericShow

duplexChromeReleaseChannel :: JsDuplex ChromeReleaseChannel String
duplexChromeReleaseChannel =
  let
    toString ChromeStable = "chrome"
    toString ChromeBeta = "chrome-beta"
    toString ChromeCanary = "chrome-canary"
    toString ChromeDev = "chrome-dev"
    fromString "chrome" = pure ChromeStable
    fromString "chrome-beta" = pure ChromeBeta
    fromString "chrome-canary" = pure ChromeCanary
    fromString "chrome-dev" = pure ChromeDev
    fromString o = Left $ "unknown chrome release channel " <> o
  in
    duplex toString fromString

type Connect =
  { defaultViewport :: Maybe Viewport
  , ignoreHTTPSErrors :: Maybe Boolean
  , protocolTimeout :: Maybe Milliseconds
  , slowMo :: Maybe Milliseconds
  }

type ConnectRaw =
  { defaultViewport :: Maybe Viewport
  , ignoreHTTPSErrors :: Maybe Boolean
  , protocolTimeout :: Maybe Number
  , slowMo :: Maybe Number
  }

duplexConnect :: JsDuplex Connect ConnectRaw
duplexConnect =
  let
    into r = modify (Proxy :: Proxy "protocolTimeout") (map unwrap)
      $ modify (Proxy :: Proxy "slowMo") (map unwrap) r
    from r = pure
      $ modify (Proxy :: Proxy "protocolTimeout") (map wrap)
      $ modify (Proxy :: Proxy "slowMo") (map wrap) r
  in
    duplex into from

foreign import _close :: Browser -> Effect (Promise Unit)
foreign import _get :: Foreign -> Effect Browser

foreign import disconnect :: Browser -> Effect Unit
foreign import websocketEndpoint :: Browser -> Effect String
foreign import connected :: Browser -> Effect Boolean
foreign import ofContext :: BrowserContext -> Effect Browser

get :: forall b. BrowserAccess b => b -> Effect Browser
get = _get <<< unsafeToForeign

close :: Browser -> Aff Unit
close = Promise.toAffE <<< _close
