module Puppeteer.Handle
  ( module X
  , id
  , HandleId(..)
  , findOne
  , findAll
  , click
  , clone
  , boundingBox
  , boxModel
  , focus
  , hover
  , isHidden
  , isVisible
  , isIntersectingViewport
  , dragToElement
  , dragToPoint
  , drop
  , screenshot
  , scrollIntoView
  , select
  , tap
  , uploadFile
  , waitForSelector
  , getProperties
  , toHTML
  , unsafeCoerceHandle
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (head)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Time (Millisecond)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Node.Buffer (Buffer)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Puppeteer.Base (Handle) as X
import Puppeteer.Base (class IsElement, Handle)
import Puppeteer.Cartesian (Coord, BoxModel, BoundingBox)
import Puppeteer.Eval as Eval
import Puppeteer.FFI as FFI
import Puppeteer.Keyboard (Key)
import Puppeteer.Screenshot (ScreenshotOptions, prepareScreenshotOptions)
import Puppeteer.Screenshot as Screenshot
import Puppeteer.Selector (class Selector)
import Puppeteer.Selector as Selector
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLElement)
import Web.HTML as HTML

id :: forall a. Handle a -> Effect HandleId
id = _id {remoteObject: HandleObject, primitive: HandlePrimitive}

data HandleId
  = HandleObject String
  | HandlePrimitive Foreign

instance eqHandleId :: Eq HandleId where
  eq (HandleObject ida) (HandleObject idb) = ida == idb
  eq (HandlePrimitive va) (HandlePrimitive vb) = writeJSON va == writeJSON vb
  eq _ _ = false

instance showHandleId :: Show HandleId where
  show (HandleObject ida) = "HandleObject " <> show ida
  show (HandlePrimitive va) = "HandlePrimitive " <> (show $ writeJSON va)

foreign import _id :: forall a. { remoteObject :: String -> HandleId, primitive :: Foreign -> HandleId } -> Handle a -> Effect HandleId
foreign import _find :: forall a b. String -> Handle a -> Promise (Array (Handle b))
foreign import _click :: forall a. Handle a -> Promise Unit
foreign import _boundingBox :: forall a. Handle a -> Promise (Nullable Foreign)
foreign import _boxModel :: forall a. Handle a -> Promise (Nullable Foreign)
foreign import _focus :: forall a. Handle a -> Promise Unit
foreign import _hover :: forall a. Handle a -> Promise Unit
foreign import _isHidden :: forall a. Handle a -> Promise Boolean
foreign import _isVisible :: forall a. Handle a -> Promise Boolean
foreign import _isIntersectingViewport :: forall a. Handle a -> Promise Boolean
foreign import _drag :: forall a. Foreign -> Handle a -> Promise Unit
foreign import _drop :: forall a b. Handle a -> Handle b -> Promise Unit
foreign import _screenshot :: forall a. Foreign -> Handle a -> Promise Buffer
foreign import _scrollIntoView :: forall a. Handle a -> Promise Unit
foreign import _select :: forall a. Array String -> Handle a -> Promise Unit
foreign import _tap :: forall a. Handle a -> Promise Unit
foreign import _uploadFile :: forall a. Array FilePath -> Handle a -> Promise Unit
foreign import _waitForSelector :: forall a b. String -> Handle a -> Promise (Handle b)
foreign import _clone :: forall a. Handle a -> Promise a
foreign import _getProperties :: forall a. Handle a -> Promise (Array { k :: String, v :: (Handle Foreign) })

clone :: forall a. WriteForeign a => ReadForeign a => Handle a -> Aff a
clone = Promise.toAff <<< _clone

findOne :: forall a b sel. IsElement a => Selector sel b => sel -> Handle a -> Aff (Maybe (Handle b))
findOne q h = do
  els <- findAll q h
  pure $ head els

findAll :: forall a b sel. IsElement a => Selector sel b => sel -> Handle a -> Aff (Array (Handle b))
findAll q h = Promise.toAff $ _find (Selector.toCSS q) h

click :: forall a. IsElement a => Handle a -> Aff Unit
click h = Promise.toAff $ _click h

boundingBox :: forall a. IsElement a => Handle a -> Aff (Maybe Foreign)
boundingBox = map Nullable.toMaybe <<< Promise.toAff <<< _boundingBox

boxModel :: forall a. IsElement a => Handle a -> Aff (Maybe Foreign)
boxModel = map Nullable.toMaybe <<< Promise.toAff <<< _boxModel

focus :: forall a. IsElement a => Handle a -> Aff Unit
focus = Promise.toAff <<< _focus

hover :: forall a. IsElement a => Handle a -> Aff Unit
hover = Promise.toAff <<< _hover

isHidden :: forall a. IsElement a => Handle a -> Aff Boolean
isHidden = Promise.toAff <<< _isHidden

isVisible :: forall a. IsElement a => Handle a -> Aff Boolean
isVisible = Promise.toAff <<< _isVisible

isIntersectingViewport :: forall a. IsElement a => Handle a -> Aff Boolean
isIntersectingViewport = Promise.toAff <<< _isIntersectingViewport

dragToPoint :: forall a. IsElement a => Coord -> Handle a -> Aff Unit
dragToPoint c = Promise.toAff <<< _drag (unsafeToForeign $ c)

dragToElement :: forall a b. IsElement a => IsElement b => Handle a -> Handle b -> Aff Unit
dragToElement from to = Promise.toAff $ _drag (unsafeToForeign from) to

drop :: forall a b. IsElement a => IsElement b => Handle a -> Handle b -> Aff Unit
drop a = Promise.toAff <<< _drop a

screenshot :: forall a. IsElement a => ScreenshotOptions -> Handle a -> Aff Buffer
screenshot o = Promise.toAff <<< _screenshot (prepareScreenshotOptions o)

scrollIntoView :: forall a. IsElement a => Handle a -> Aff Unit
scrollIntoView = Promise.toAff <<< _scrollIntoView

select :: forall a. IsElement a => Array String -> Handle a -> Aff Unit
select a = Promise.toAff <<< _select a

tap :: forall a. IsElement a => Handle a -> Aff Unit
tap = Promise.toAff <<< _tap

uploadFile :: Array FilePath -> Handle HTML.HTMLInputElement -> Aff Unit
uploadFile a = Promise.toAff <<< _uploadFile a

waitForSelector :: forall a b s. Selector s b => IsElement a => s -> Handle a -> Aff (Handle b)
waitForSelector s = Promise.toAff <<< _waitForSelector (Selector.toCSS s)

getProperties :: forall a. Handle a -> Aff (Map String (Handle Foreign))
getProperties = map FFI.makeMap <<< Promise.toAff <<< _getProperties

toHTML :: forall a. Handle a -> Aff (Maybe (Handle HTMLElement))
toHTML h = do
  isHtml <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLElement" h
  pure $ if isHtml then Just (unsafeCoerceHandle h) else Nothing

unsafeCoerceHandle :: forall a b. Handle a -> Handle b
unsafeCoerceHandle = unsafeCoerce
