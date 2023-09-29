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
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (Context(..), Page, closeContext)
import Puppeteer.FFI as FFI
import Puppeteer.HTTP (Request)
import Puppeteer.Page.Event as Event

foreign import _bypassCsp :: Page -> Promise Unit
foreign import _unbypassCsp :: Page -> Promise Unit

foreign import _enableCache :: Page -> Promise Unit
foreign import _disableCache :: Page -> Promise Unit

foreign import _interceptRequests :: Page -> Promise Unit
foreign import _uninterceptRequests :: Page -> Promise Unit

foreign import _sendExtraHeaders :: Foreign -> Page -> Promise Unit

type BypassCSPHint :: Symbol
type BypassCSPHint = "CSP is being bypassed. Invoking `Puppeteer.closeContext` will restore default CSP behavior."

type DisableCacheHint :: Symbol
type DisableCacheHint = "Caching is disabled. Invoking `Puppeteer.closeContext` will restore default cache behavior."

type InterceptRequestsHint :: Symbol
type InterceptRequestsHint = "Requests are being intercepted. Invoking `Puppeteer.closeContext` will restore normal request behavior."

bypassCsp :: Page -> Aff (Context BypassCSPHint)
bypassCsp p = do
  Promise.toAff $ _bypassCsp p
  pure $ Context (\_ -> Promise.toAff $ _unbypassCsp p)

disableCache :: Page -> Aff (Context DisableCacheHint)
disableCache p = do
  Promise.toAff $ _disableCache p
  pure $ Context (\_ -> Promise.toAff $ _enableCache p)

interceptRequests :: Page -> Aff (Context InterceptRequestsHint)
interceptRequests p = do
  Promise.toAff $ _interceptRequests p
  pure (Context $ \_ -> Promise.toAff $ _uninterceptRequests p)

interceptNextRequest :: (Context InterceptRequestsHint -> Request -> Aff Unit) -> Page -> Aff Unit
interceptNextRequest cb p = do
  ctx <- interceptRequests p
  req <- Event.once Event.Request p
  cb ctx req
  closeContext ctx
  pure unit

sendExtraHeaders :: Map String String -> Page -> Aff Unit
sendExtraHeaders h = Promise.toAff <<< _sendExtraHeaders (FFI.mapToRecord h)
