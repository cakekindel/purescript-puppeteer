module Puppeteer.Page
  ( module X
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
  , ScriptType(..)
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
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Node.Path (FilePath)
import Puppeteer.Base (Page) as X
import Puppeteer.Base (class PageProducer, Handle, Keyboard, LifecycleEvent, Page, URL, Viewport, prepareLifecycleEvent, prepareViewport)
import Puppeteer.Handle (unsafeCoerceHandle)
import Puppeteer.Selector (class Selector, toCSS)
import Simple.JSON (readImpl, undefined, writeImpl)
import Web.HTML (HTMLLinkElement, HTMLScriptElement, HTMLStyleElement)

data ScriptType = Script | Module

prepareScriptType :: ScriptType -> Foreign
prepareScriptType Module = unsafeToForeign "module"
prepareScriptType Script = undefined

data AddScript
  = AddScriptInline ScriptType String
  | AddScriptLocal ScriptType FilePath
  | AddScriptRemote ScriptType URL

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
prepareAddScript (AddScriptInline type' content') = writeImpl
  { type: prepareScriptType type'
  , content: content'
  }
prepareAddScript (AddScriptLocal type' path) = writeImpl
  { type: prepareScriptType type'
  , path
  }
prepareAddScript (AddScriptRemote type' url') = writeImpl
  { type: prepareScriptType type'
  , url: url'
  }

foreign import url :: Page -> Effect URL
foreign import keyboard :: Page -> Effect Keyboard
foreign import mouse :: Page -> Effect Unit
foreign import touchscreen :: Page -> Effect Unit
foreign import isClosed :: Page -> Effect Boolean

foreign import _newPage :: Foreign -> Promise Page
foreign import _all :: Foreign -> Promise (Array Page)
foreign import _findAll :: forall a. String -> Page -> Promise (Array (Handle a))
foreign import _addScriptTag :: Foreign -> Page -> Promise (Handle HTMLScriptElement)
foreign import _addStyleTag :: Foreign -> Page -> Promise (Handle Foreign)
foreign import _bringToFront :: Page -> Promise Unit
foreign import _close :: Page -> Promise Unit
foreign import _content :: Page -> Promise String
foreign import _setContent :: String -> Foreign -> Page -> Promise Unit
foreign import _setViewport :: Foreign -> Page -> Promise Unit
foreign import _title :: Page -> Promise String
foreign import _viewport :: Page -> Foreign

new :: forall b. PageProducer b => b -> Aff Page
new = Promise.toAff <<< _newPage <<< unsafeToForeign

all :: forall b. PageProducer b => b -> Aff (Array Page)
all = Promise.toAff <<< _all <<< unsafeToForeign

findAll :: forall s e. Selector s e => s -> Page -> Aff (Array (Handle e))
findAll s = Promise.toAff <<< _findAll (toCSS s)

findFirst :: forall s e. Selector s e => s -> Page -> Aff (Maybe (Handle e))
findFirst s = map Array.head <<< findAll s

addScriptTag :: AddScript -> Page -> Aff (Handle HTMLScriptElement)
addScriptTag a = Promise.toAff <<< _addScriptTag (prepareAddScript a)

addStyleTag :: forall s e. AddStyle s e => s -> Page -> Aff (Handle e)
addStyleTag a h = do
  t <- Promise.toAff $ _addStyleTag (prepareAddStyle a) h
  pure $ unsafeCoerceHandle t

bringToFront :: Page -> Aff Unit
bringToFront = Promise.toAff <<< _bringToFront

close :: Page -> Aff Unit
close = Promise.toAff <<< _close

content :: Page -> Aff String
content = Promise.toAff <<< _content

setContent :: String  -> LifecycleEvent -> Page -> Aff Unit
setContent s ev = Promise.toAff <<< _setContent s (prepareLifecycleEvent ev)

setViewport :: Viewport -> Page -> Aff Unit
setViewport vp = Promise.toAff <<< _setViewport (prepareViewport vp)

title :: Page -> Aff String
title = Promise.toAff <<< _title

viewport :: Page -> Maybe Viewport
viewport = hush <<< runExcept <<< readImpl <<< _viewport
