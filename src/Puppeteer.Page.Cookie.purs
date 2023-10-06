module Puppeteer.Page.Cookie
  ( set
  , listForCurrentPage
  , listForUrl
  , listForUrls
  , delete
  , Cookie
  , CookieSet
  , CookieDelete
  , CookieSameSite(..)
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (Page, URL)
import Puppeteer.FFI as FFI
import Simple.JSON (writeImpl)

foreign import _list :: Array String -> Page -> Effect (Promise (Array CookieRaw))
foreign import _delete :: Foreign -> Page -> Effect (Promise Unit)
foreign import _set :: Foreign -> Page -> Effect (Promise Unit)

data CookieSameSite = Strict | Lax | None

type CookieRaw =
  { name :: String
  , value :: String
  , url :: URL
  , domain :: String
  , path :: String
  , secure :: Boolean
  , sameSite :: String
  , expires :: Number
  }

type Cookie =
  { name :: String
  , value :: String
  , url :: URL
  , domain :: String
  , path :: String
  , secure :: Boolean
  , sameSite :: CookieSameSite
  , expires :: Instant
  }

type CookieSet =
  { name :: String
  , value :: String
  , url :: Maybe URL
  , domain :: Maybe String
  , path :: Maybe String
  , secure :: Maybe Boolean
  , sameSite :: Maybe CookieSameSite
  , expires :: Maybe Instant
  }

type CookieDelete =
  { name :: String
  , url :: Maybe URL
  , domain :: Maybe String
  , path :: Maybe String
  }

prepareSameSite :: CookieSameSite -> Foreign
prepareSameSite Strict = writeImpl "Strict"
prepareSameSite Lax = writeImpl "Lax"
prepareSameSite None = writeImpl "None"

set :: CookieSet -> Page -> Aff Unit
set { name, value, url, domain, path, secure, sameSite, expires } p =
  let
    o = writeImpl
      { name
      , value
      , secure: FFI.maybeToUndefined secure
      , sameSite: FFI.maybeToUndefined $ map prepareSameSite sameSite
      , url: FFI.maybeToUndefined url
      , domain: FFI.maybeToUndefined domain
      , path: FFI.maybeToUndefined path
      , expires: map (unwrap <<< Instant.unInstant) expires
      }
  in
    Promise.toAffE $ _set o p

delete :: CookieDelete -> Page -> Aff Unit
delete { name, url, domain, path } p =
  let
    o = writeImpl { name, url: FFI.maybeToUndefined url, domain: FFI.maybeToUndefined domain, path: FFI.maybeToUndefined path }
  in
    Promise.toAffE $ _delete o p

listForCurrentPage :: Page -> Aff (Array Cookie)
listForCurrentPage = map (Array.catMaybes <<< map cookieRaw) <<< Promise.toAffE <<< _list []

listForUrl :: URL -> Page -> Aff (Array Cookie)
listForUrl = listForUrls <<< pure

listForUrls :: NonEmptyArray URL -> Page -> Aff (Array Cookie)
listForUrls urls p = map (Array.catMaybes <<< map cookieRaw) $ Promise.toAffE $ _list (Array.NonEmpty.toArray urls) p

cookieRaw :: CookieRaw -> Maybe Cookie
cookieRaw { domain, expires, path, url, name, value, sameSite, secure } = do
  expires' <- Instant.instant $ wrap expires
  sameSite' <- case sameSite of
    "Strict" -> Just Strict
    "Lax" -> Just Lax
    "None" -> Just None
    _ -> Nothing
  pure { domain, expires: expires', path, url, name, value, sameSite: sameSite', secure }
