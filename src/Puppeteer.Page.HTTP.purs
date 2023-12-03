module Puppeteer.Page.HTTP
  ( BypassCSPHint
  , InterceptRequestsHint
  , DisableCacheHint
  , bypassCsp
  , disableCache
  , interceptRequests
  , sendExtraHeaders
  , interceptNextRequest
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Map (Map)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (Context(..), Page, closeContext)
import Puppeteer.FFI as FFI
import Puppeteer.HTTP (Request)
import Puppeteer.Page.Event as Event

foreign import _bypassCsp :: Page -> Effect (Promise Unit)
foreign import _unbypassCsp :: Page -> Effect (Promise Unit)

foreign import _enableCache :: Page -> Effect (Promise Unit)
foreign import _disableCache :: Page -> Effect (Promise Unit)

foreign import _interceptRequests :: Page -> Effect (Promise Unit)
foreign import _uninterceptRequests :: Page -> Effect (Promise Unit)

foreign import _sendExtraHeaders :: Foreign -> Page -> Effect (Promise Unit)

type BypassCSPHint :: Symbol
type BypassCSPHint = "CSP is being bypassed. Invoking `Puppeteer.closeContext` will restore default CSP behavior."

type DisableCacheHint :: Symbol
type DisableCacheHint = "Caching is disabled. Invoking `Puppeteer.closeContext` will restore default cache behavior."

type InterceptRequestsHint :: Symbol
type InterceptRequestsHint = "Requests are being intercepted. Invoking `Puppeteer.closeContext` will restore normal request behavior."

bypassCsp :: Page -> Aff (Context BypassCSPHint)
bypassCsp p = do
  FFI.promiseToAff $ _bypassCsp p
  pure $ Context (\_ -> FFI.promiseToAff $ _unbypassCsp p)

disableCache :: Page -> Aff (Context DisableCacheHint)
disableCache p = do
  FFI.promiseToAff $ _disableCache p
  pure $ Context (\_ -> FFI.promiseToAff $ _enableCache p)

interceptRequests :: Page -> Aff (Context InterceptRequestsHint)
interceptRequests p = do
  FFI.promiseToAff $ _interceptRequests p
  pure (Context $ \_ -> FFI.promiseToAff $ _uninterceptRequests p)

interceptNextRequest :: (Context InterceptRequestsHint -> Request -> Aff Unit) -> Page -> Aff Unit
interceptNextRequest cb p = do
  ctx <- interceptRequests p
  req <- Event.once Event.Request p
  cb ctx req
  closeContext ctx
  pure unit

sendExtraHeaders :: Map String String -> Page -> Aff Unit
sendExtraHeaders h = FFI.promiseToAff <<< _sendExtraHeaders (FFI.mapToRecord h)
