module Puppeteer.Browser.Context
  ( module X
  , all
  , default
  , close
  , incognito
  , incognito_
  , overridePermissions
  , clearPermissionOverrides
  , isIncognito
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (BrowserContext) as X
import Puppeteer.Base (BrowserContext, Page)
import Puppeteer.Browser (Browser)
import Puppeteer.Browser.Permission (Permission)
import Puppeteer.FFI as FFI
import Simple.JSON (writeImpl)

type Create =
  { proxyBypassList :: Maybe (Array String)
  , proxyServer :: Maybe String
  }

prepareCreate :: Create -> Foreign
prepareCreate
  { proxyBypassList
  , proxyServer
  } =
  writeImpl
    { proxyBypassList: FFI.maybeToUndefined proxyBypassList
    , proxyServer: FFI.maybeToUndefined proxyServer
    }

foreign import all :: Browser -> Array BrowserContext
foreign import isIncognito :: BrowserContext -> Boolean
foreign import forPage :: Page -> BrowserContext

foreign import _default :: Browser -> BrowserContext
foreign import _incognito :: Foreign -> Browser -> Effect (Promise BrowserContext)
foreign import _overridePermissions :: String -> Array Permission -> BrowserContext -> Effect (Promise Unit)
foreign import _clearPermissionOverrides :: BrowserContext -> Effect (Promise Unit)
foreign import _close :: BrowserContext -> Effect (Promise Unit)

incognito :: Create -> Browser -> Aff BrowserContext
incognito c b = Promise.toAffE $ _incognito (prepareCreate c) b

incognito_ :: Browser -> Aff BrowserContext
incognito_ = incognito { proxyBypassList: Nothing, proxyServer: Nothing }

default :: Browser -> BrowserContext
default = _default

overridePermissions :: String -> Set Permission -> BrowserContext -> Aff Unit
overridePermissions origin perms ctx = Promise.toAffE $ _overridePermissions origin (Set.toUnfoldable perms) ctx

clearPermissionOverrides :: BrowserContext -> Aff Unit
clearPermissionOverrides = Promise.toAffE <<< _clearPermissionOverrides

close :: BrowserContext -> Aff Unit
close = Promise.toAffE <<< _close
