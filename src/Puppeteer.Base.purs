module Puppeteer.Base where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Except (runExcept)
import Control.Parallel (parallel, sequential)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Aff, delay)
import Effect.Exception (Error, error)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Union)
import Puppeteer.FFI as FFI
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Web.HTML as HTML

foreign import unsafeLog :: forall a. a -> a
foreign import unsafeUnion :: forall a b c. a -> b -> c

data JsDuplex a ir = JsDuplex
  { from :: ir -> Either String a
  , into :: a -> ir
  }

duplex :: forall a ir. (a -> ir) -> (ir -> Either String a) -> JsDuplex a ir
duplex into from = JsDuplex { from, into }

duplexRead :: forall a ir. ReadForeign ir => JsDuplex a ir -> Foreign -> Either Error a
duplexRead (JsDuplex { from }) = lmap error <<< flip bind from <<< lmap show <<< runExcept <<< readImpl

duplexWrite :: forall a ir. WriteForeign ir => JsDuplex a ir -> a -> Foreign
duplexWrite (JsDuplex { into }) = writeImpl <<< into

mapToObject :: forall v. WriteForeign v => Map String v -> Object Foreign
mapToObject = Object.fromFoldableWithIndex <<< map writeImpl

merge :: forall a b c. Union a b c => Record a -> Record b -> Record c
merge a b = unsafeUnion a b

timeout :: forall a. Milliseconds -> Aff a -> Aff (Maybe a)
timeout t a =
  let
    timeout_ = const Nothing <$> delay t
  in
    sequential $ parallel (hush <$> try a) <|> parallel timeout_

timeoutThrow :: forall a. Milliseconds -> Aff a -> Aff a
timeoutThrow t a = liftMaybe (error "timeout") =<< timeout t a

newtype Context (a :: Symbol) = Context (Unit -> Aff Unit)

instance Semigroup (Context a) where
  append (Context a) (Context b) = Context (b <=< a)

instance Monoid (Context a) where
  mempty = Context $ const $ pure unit

closeContext :: forall (a :: Symbol). Context a -> Aff Unit
closeContext (Context f) = f unit

type URL = String

type Viewport =
  { deviceScaleFactor :: Maybe Number
  , hasTouch :: Maybe Boolean
  , height :: Int
  , width :: Int
  , isLandscape :: Maybe Boolean
  , isMobile :: Maybe Boolean
  }

duplexViewport :: JsDuplex Viewport Viewport
duplexViewport = duplex identity pure

--| [`PuppeteerNode`](https://pptr.dev/api/puppeteer.puppeteernode)
foreign import data Puppeteer :: Row Type -> Type

--| [`CDPSession`](https://pptr.dev/api/puppeteer.cdpsession)
foreign import data CDPSession :: Type

data LifecycleEvent = Load | DomContentLoaded | NetworkIdleZeroConnections | NetworkIdleAtMostTwoConnections

duplexLifecycleEvent :: JsDuplex LifecycleEvent String
duplexLifecycleEvent =
  let
    toString Load = "load"
    toString DomContentLoaded = "domcontentloaded"
    toString NetworkIdleZeroConnections = "networkidle0"
    toString NetworkIdleAtMostTwoConnections = "networkidle2"
    fromString "load" = Right Load
    fromString "domcontentloaded" = Right DomContentLoaded
    fromString "networkidle0" = Right NetworkIdleZeroConnections
    fromString "networkidle2" = Right NetworkIdleAtMostTwoConnections
    fromString o = Left $ "unknown lifecycle event " <> o
  in
    duplex toString fromString

--| [`Browser`](https://pptr.dev/api/puppeteer.browser)
foreign import data Browser :: Type

instance ReadForeign Browser where
  readImpl = pure <<< unsafeFromForeign

--| [`Page`](https://pptr.dev/api/puppeteer.page)
foreign import data Page :: Type

instance ReadForeign Page where
  readImpl = pure <<< unsafeFromForeign

--| [`Frame`](https://pptr.dev/api/puppeteer.frame)
foreign import data Frame :: Type

instance ReadForeign Frame where
  readImpl = pure <<< unsafeFromForeign

--| [`BrowserContext`](https://pptr.dev/api/puppeteer.browsercontext)
foreign import data BrowserContext :: Type

instance ReadForeign BrowserContext where
  readImpl = pure <<< unsafeFromForeign

--| Represents both [`JSHandle`](https://pptr.dev/api/puppeteer.jshandle) & [`ElementHandle`](https://pptr.dev/api/puppeteer.elementhandle)
foreign import data Handle :: Type -> Type

--| [`Keyboard`](https://pptr.dev/api/puppeteer.keyboard)
foreign import data Keyboard :: Type

instance ReadForeign Keyboard where
  readImpl = pure <<< unsafeFromForeign

foreign import data Request :: Type

instance ReadForeign Request where
  readImpl = pure <<< unsafeFromForeign

foreign import data Response :: Type

instance ReadForeign Response where
  readImpl = pure <<< unsafeFromForeign

--| `Browser` or `BrowserContext`
class PageProducer :: Type -> Constraint
class PageProducer a

instance PageProducer Browser
instance PageProducer BrowserContext

--| `Page` or `Handle`
class EvalTarget :: Type -> Constraint
class EvalTarget a

instance EvalTarget Page
instance EvalTarget (Handle a)

--| `Page` or `BrowserContext`
class BrowserAccess :: Type -> Constraint
class BrowserAccess a

instance BrowserAccess Browser
instance BrowserAccess BrowserContext

class IsElement :: Type -> Constraint
class IsElement e

instance IsElement HTML.HTMLAnchorElement
instance IsElement HTML.HTMLAreaElement
instance IsElement HTML.HTMLAudioElement
instance IsElement HTML.HTMLBRElement
instance IsElement HTML.HTMLBaseElement
instance IsElement HTML.HTMLBodyElement
instance IsElement HTML.HTMLButtonElement
instance IsElement HTML.HTMLCanvasElement
instance IsElement HTML.HTMLDListElement
instance IsElement HTML.HTMLDataElement
instance IsElement HTML.HTMLDataListElement
instance IsElement HTML.HTMLDivElement
instance IsElement HTML.HTMLDocument
instance IsElement HTML.HTMLElement
instance IsElement HTML.HTMLEmbedElement
instance IsElement HTML.HTMLFieldSetElement
instance IsElement HTML.HTMLFormElement
instance IsElement HTML.HTMLHRElement
instance IsElement HTML.HTMLHeadElement
instance IsElement HTML.HTMLHeadingElement
instance IsElement HTML.HTMLIFrameElement
instance IsElement HTML.HTMLImageElement
instance IsElement HTML.HTMLInputElement
instance IsElement HTML.HTMLKeygenElement
instance IsElement HTML.HTMLLIElement
instance IsElement HTML.HTMLLabelElement
instance IsElement HTML.HTMLLegendElement
instance IsElement HTML.HTMLLinkElement
instance IsElement HTML.HTMLMapElement
instance IsElement HTML.HTMLMediaElement
instance IsElement HTML.HTMLMetaElement
instance IsElement HTML.HTMLMeterElement
instance IsElement HTML.HTMLModElement
instance IsElement HTML.HTMLOListElement
instance IsElement HTML.HTMLObjectElement
instance IsElement HTML.HTMLOptGroupElement
instance IsElement HTML.HTMLOptionElement
instance IsElement HTML.HTMLOutputElement
instance IsElement HTML.HTMLParagraphElement
instance IsElement HTML.HTMLParamElement
instance IsElement HTML.HTMLPreElement
instance IsElement HTML.HTMLProgressElement
instance IsElement HTML.HTMLQuoteElement
instance IsElement HTML.HTMLScriptElement
instance IsElement HTML.HTMLSelectElement
instance IsElement HTML.HTMLSourceElement
instance IsElement HTML.HTMLSpanElement
instance IsElement HTML.HTMLStyleElement
instance IsElement HTML.HTMLTableCaptionElement
instance IsElement HTML.HTMLTableCellElement
instance IsElement HTML.HTMLTableColElement
instance IsElement HTML.HTMLTableDataCellElement
instance IsElement HTML.HTMLTableElement
instance IsElement HTML.HTMLTableHeaderCellElement
instance IsElement HTML.HTMLTableRowElement
instance IsElement HTML.HTMLTableSectionElement
instance IsElement HTML.HTMLTemplateElement
instance IsElement HTML.HTMLTextAreaElement
instance IsElement HTML.HTMLTimeElement
instance IsElement HTML.HTMLTitleElement
instance IsElement HTML.HTMLTrackElement
instance IsElement HTML.HTMLUListElement
instance IsElement HTML.HTMLVideoElement
