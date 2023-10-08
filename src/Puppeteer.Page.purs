module Puppeteer.Page
  ( module X
  , authenticate
  , new
  , all
  , findAll
  , findFirst
  , addScriptTag
  , addStyleTag
  , bringToFront
  , class AddStyle
  , prepareAddStyle
  , AddStyleInline(..)
  , AddStyleLocal(..)
  , AddStyleRemote(..)
  , AddScript(..)
  , close
  , isClosed
  , content
  , setContent
  , setViewport
  , title
  , url
  , viewport
  , keyboard
  , mouse
  , touchscreen
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Node.Path (FilePath)
import Puppeteer.Base (Page) as X
import Puppeteer.Base (class PageProducer, Handle, Keyboard, LifecycleEvent, Page, URL, Viewport, duplexLifecycleEvent, duplexViewport, duplexWrite)
import Puppeteer.Handle (unsafeCoerceHandle)
import Puppeteer.Selector (class Selector, toCSS)
import Simple.JSON (readImpl, undefined, writeImpl)
import Web.HTML (HTMLLinkElement, HTMLScriptElement, HTMLStyleElement)

data AddScript
  = AddScriptInline String
  | AddScriptLocal FilePath
  | AddScriptRemote URL
  | AddModuleInline String
  | AddModuleLocal FilePath
  | AddModuleRemote URL

data AddStyleInline = AddStyleInline String
data AddStyleLocal = AddStyleLocal FilePath
data AddStyleRemote = AddStyleRemote URL

class AddStyle :: Type -> Type -> Constraint
class AddStyle a r | a -> r where
  prepareAddStyle :: a -> Foreign

instance styleInline :: AddStyle AddStyleInline HTMLStyleElement where
  prepareAddStyle (AddStyleInline content') = writeImpl { content: content' }

instance styleLocal :: AddStyle AddStyleLocal HTMLStyleElement where
  prepareAddStyle (AddStyleLocal path) = writeImpl { path }

instance styleRemote :: AddStyle AddStyleRemote HTMLLinkElement where
  prepareAddStyle (AddStyleRemote url') = writeImpl { url: url' }

prepareAddScript :: AddScript -> Foreign
prepareAddScript (AddScriptInline content') = writeImpl
  { type: undefined
  , content: content'
  }
prepareAddScript (AddScriptLocal path) = writeImpl
  { type: undefined
  , path
  }
prepareAddScript (AddScriptRemote url') = writeImpl
  { type: undefined
  , url: url'
  }
prepareAddScript (AddModuleInline content') = writeImpl
  { type: writeImpl "module"
  , content: content'
  }
prepareAddScript (AddModuleLocal path) = writeImpl
  { type: writeImpl "module"
  , path
  }
prepareAddScript (AddModuleRemote url') = writeImpl
  { type: writeImpl "module"
  , url: url'
  }

foreign import url :: Page -> Effect URL
foreign import keyboard :: Page -> Effect Keyboard
foreign import mouse :: Page -> Effect Unit
foreign import touchscreen :: Page -> Effect Unit
foreign import isClosed :: Page -> Effect Boolean

foreign import _authenticate :: { username :: String, password :: String } -> Page -> Effect (Promise Unit)
foreign import _newPage :: Foreign -> Effect (Promise Page)
foreign import _all :: Foreign -> Effect (Promise (Array Page))
foreign import _findAll :: forall a. String -> Page -> Effect (Promise (Array (Handle a)))
foreign import _addScriptTag :: Foreign -> Page -> Effect (Promise (Handle HTMLScriptElement))
foreign import _addStyleTag :: Foreign -> Page -> Effect (Promise (Handle Foreign))
foreign import _bringToFront :: Page -> Effect (Promise Unit)
foreign import _close :: Page -> Effect (Promise Unit)
foreign import _content :: Page -> Effect (Promise String)
foreign import _setContent :: String -> Foreign -> Page -> Effect (Promise Unit)
foreign import _setViewport :: Foreign -> Page -> Effect (Promise Unit)
foreign import _title :: Page -> Effect (Promise String)
foreign import _viewport :: Page -> Foreign

new :: forall b. PageProducer b => b -> Aff Page
new = Promise.toAffE <<< _newPage <<< unsafeToForeign

authenticate :: { username :: String, password :: String } -> Page -> Aff Unit
authenticate creds = Promise.toAffE <<< _authenticate creds

all :: forall b. PageProducer b => b -> Aff (Array Page)
all = Promise.toAffE <<< _all <<< unsafeToForeign

findAll :: forall s e. Selector s e => s -> Page -> Aff (Array (Handle e))
findAll s = Promise.toAffE <<< _findAll (toCSS s)

findFirst :: forall s e. Selector s e => s -> Page -> Aff (Maybe (Handle e))
findFirst s = map Array.head <<< findAll s

addScriptTag :: AddScript -> Page -> Aff (Handle HTMLScriptElement)
addScriptTag a = Promise.toAffE <<< _addScriptTag (prepareAddScript a)

addStyleTag :: forall s e. AddStyle s e => s -> Page -> Aff (Handle e)
addStyleTag a h = do
  t <- Promise.toAffE $ _addStyleTag (prepareAddStyle a) h
  pure $ unsafeCoerceHandle t

bringToFront :: Page -> Aff Unit
bringToFront = Promise.toAffE <<< _bringToFront

close :: Page -> Aff Unit
close = Promise.toAffE <<< _close

content :: Page -> Aff String
content = Promise.toAffE <<< _content

setContent :: String -> LifecycleEvent -> Page -> Aff Unit
setContent s ev = Promise.toAffE <<< _setContent s (duplexWrite duplexLifecycleEvent ev)

setViewport :: Viewport -> Page -> Aff Unit
setViewport vp = Promise.toAffE <<< _setViewport (duplexWrite duplexViewport vp)

title :: Page -> Aff String
title = Promise.toAffE <<< _title

viewport :: Page -> Maybe Viewport
viewport = hush <<< runExcept <<< readImpl <<< _viewport
