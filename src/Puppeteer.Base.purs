module Puppeteer.Base where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Parallel (parallel, sequential)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Aff, delay)
import Effect.Exception (error)
import Foreign (Foreign, unsafeFromForeign)
import Prim.Row (class Union)
import Puppeteer.FFI as FFI
import Simple.JSON (class ReadForeign, writeImpl)
import Web.HTML as HTML

foreign import unsafeLog :: forall a. a -> a

timeout :: forall a. Milliseconds -> Aff a -> Aff (Maybe a)
timeout t a =
  let
    timeout_ = const Nothing <$> delay t
  in
    sequential $ parallel (hush <$> try a) <|> parallel timeout_

timeoutThrow :: forall a. Milliseconds -> Aff a -> Aff a
timeoutThrow t a = liftMaybe (error "timeout") =<< timeout t a

newtype Context (a :: Symbol) = Context (Unit -> Aff Unit)

instance semicontext :: Semigroup (Context a) where
  append _ a = a

instance monoidcontext :: Monoid (Context a) where
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

prepareViewport :: Viewport -> Foreign
prepareViewport { deviceScaleFactor, hasTouch, height, width, isLandscape, isMobile } =
  writeImpl
    { deviceScaleFactor: FFI.maybeToUndefined deviceScaleFactor
    , hasTouch: FFI.maybeToUndefined hasTouch
    , isLandscape: FFI.maybeToUndefined isLandscape
    , isMobile: FFI.maybeToUndefined isMobile
    , height
    , width
    }

--| [`PuppeteerNode`](https://pptr.dev/api/puppeteer.puppeteernode)
foreign import data Puppeteer :: Row Type -> Type

data LifecycleEvent = Load | DomContentLoaded | NetworkIdleZeroConnections | NetworkIdleAtMostTwoConnections

prepareLifecycleEvent :: LifecycleEvent -> Foreign
prepareLifecycleEvent Load = writeImpl "load"
prepareLifecycleEvent DomContentLoaded = writeImpl "domcontentloaded"
prepareLifecycleEvent NetworkIdleZeroConnections = writeImpl "networkidle0"
prepareLifecycleEvent NetworkIdleAtMostTwoConnections = writeImpl "networkidle2"

--| A puppeteer plugin
--|
--| [`puppeteer-extra`](https://github.com/berstend/puppeteer-extra/tree/master/packages/puppeteer-extra-plugin)
--|
--| `src/DebugPlugin.js`
--| ```javascript
--| import { PuppeteerExtraPlugin } from 'puppeteer-extra-plugin'
--| import { PuppeteerExtra } from 'puppeteer-extra'
--| import { Page } from 'puppeteer'
--|
--| /** @typedef {Page & {sayHello: () => void}} DebugPluginPage */
--|
--| class DebugPlugin extends PuppeteerExtraPlugin {
--|   name = 'hello-world'
--|
--|   constructor(opts = {}) {
--|     super(opts)
--|   }
--| 
--|   async onPageCreated(page) {
--|     page.sayHello = () => console.log('hello')
--|   }
--| }
--|
--| /** @type {() => DebugPlugin} */
--| export const makeDebugPlugin = () => new DebugPlugin()
--|
--| /** @type {(_1: DebugPlugin) => (_2: PuppeteerExtra) => () => PuppeteerExtra} */
--| export const registerDebugPlugin = dp => p => () => p.use(dp)
--|
--| /** @type {(_1: PuppeteerExtra) => (_2: DebugPluginPage) => () => void} */
--| export const sayHello = () => page => () => page.sayHello()
--| ```
--|
--| `src/DebugPlugin.purs`
--| ```purescript
--| module DebugPlugin where
--|
--| import Prelude
--| import Effect (Effect)
--| import Effect.Class (class MonadEffect)
--| import Puppeteer (class Plugin, Puppeteer)
--| import Puppeteer.Page (Page)
--|
--| foreign import data DebugPlugin :: Type
--|
--| foreign import makeDebugPlugin :: Effect DebugPlugin
--|
--| foreign import registerDebugPlugin :: forall (r :: Row Type)
--|                                     . Puppeteer r
--|                                    -> Effect (Puppeteer (debugPlugin :: DebugPlugin | r))
--|
--| -- Note:
--| --  The puppeteer instance used here must have been
--| --  registered with `DebugPlugin`'s `use` in order to
--| --  invoke `sayHello`
--| foreign import sayHello :: forall (r :: Row Type)
--|                          . Puppeteer (debugPlugin :: DebugPlugin | r)
--|                         -> Page
--|                         -> Effect Unit
--|
--| instance debugPlugin :: Plugin DebugPlugin (debugPlugin :: DebugPlugin) where
--|   use pptr _ = liftEffect $ registerDebugPlugin pptr
--| ```
class Plugin p (r :: Row Type) | p -> r where
  --| Register a given puppeteer instance with plugin `p`
  --|
  --| The row type `r` should be used in that plugin's purescript
  --| API to ensure the puppeteer instance used has had that
  --| plugin registered.
  use :: forall b c. Union r b c => Puppeteer r -> p -> Aff (Puppeteer c)

--| [`Browser`](https://pptr.dev/api/puppeteer.browser)
foreign import data Browser :: Type

instance browserForeign :: ReadForeign Browser where
  readImpl = pure <<< unsafeFromForeign

--| [`Page`](https://pptr.dev/api/puppeteer.page)
foreign import data Page :: Type

instance pageForeign :: ReadForeign Page where
  readImpl = pure <<< unsafeFromForeign

--| [`Frame`](https://pptr.dev/api/puppeteer.frame)
foreign import data Frame :: Type

instance frameForeign :: ReadForeign Frame where
  readImpl = pure <<< unsafeFromForeign

--| [`BrowserContext`](https://pptr.dev/api/puppeteer.browsercontext)
foreign import data BrowserContext :: Type

instance browserContextForeign :: ReadForeign BrowserContext where
  readImpl = pure <<< unsafeFromForeign

--| Represents both [`JSHandle`](https://pptr.dev/api/puppeteer.jshandle) & [`ElementHandle`](https://pptr.dev/api/puppeteer.elementhandle)
foreign import data Handle :: Type -> Type

--| [`Keyboard`](https://pptr.dev/api/puppeteer.keyboard)
foreign import data Keyboard :: Type

foreign import data Request :: Type

instance foreignRequest :: ReadForeign Request where
  readImpl = pure <<< unsafeFromForeign

foreign import data Response :: Type

instance foreignResponse :: ReadForeign Response where
  readImpl = pure <<< unsafeFromForeign

--| `Browser` or `BrowserContext`
class PageProducer :: Type -> Constraint
class PageProducer a

instance bpp :: PageProducer Browser
instance bcpp :: PageProducer BrowserContext

--| `Page` or `Handle`
class EvalTarget :: Type -> Constraint
class EvalTarget a

instance pet :: EvalTarget Page
instance het :: EvalTarget (Handle a)

--| `Page` or `BrowserContext`
class BrowserAccess :: Type -> Constraint
class BrowserAccess a

instance pba :: BrowserAccess Browser
instance bcba :: BrowserAccess BrowserContext

class IsElement :: Type -> Constraint
class IsElement e

instance anchorIsElement :: IsElement HTML.HTMLAnchorElement
instance areaIsElement :: IsElement HTML.HTMLAreaElement
instance audioIsElement :: IsElement HTML.HTMLAudioElement
instance bRIsElement :: IsElement HTML.HTMLBRElement
instance baseIsElement :: IsElement HTML.HTMLBaseElement
instance bodyIsElement :: IsElement HTML.HTMLBodyElement
instance buttonIsElement :: IsElement HTML.HTMLButtonElement
instance canvasIsElement :: IsElement HTML.HTMLCanvasElement
instance dListIsElement :: IsElement HTML.HTMLDListElement
instance dataIsElement :: IsElement HTML.HTMLDataElement
instance dataListIsElement :: IsElement HTML.HTMLDataListElement
instance divIsElement :: IsElement HTML.HTMLDivElement
instance document :: IsElement HTML.HTMLDocument
instance element :: IsElement HTML.HTMLElement
instance embedIsElement :: IsElement HTML.HTMLEmbedElement
instance fieldSetIsElement :: IsElement HTML.HTMLFieldSetElement
instance formIsElement :: IsElement HTML.HTMLFormElement
instance hRIsElement :: IsElement HTML.HTMLHRElement
instance headIsElement :: IsElement HTML.HTMLHeadElement
instance headingIsElement :: IsElement HTML.HTMLHeadingElement
instance iFrameIsElement :: IsElement HTML.HTMLIFrameElement
instance imageIsElement :: IsElement HTML.HTMLImageElement
instance inputIsElement :: IsElement HTML.HTMLInputElement
instance keygenIsElement :: IsElement HTML.HTMLKeygenElement
instance lIIsElement :: IsElement HTML.HTMLLIElement
instance labelIsElement :: IsElement HTML.HTMLLabelElement
instance legendIsElement :: IsElement HTML.HTMLLegendElement
instance linkIsElement :: IsElement HTML.HTMLLinkElement
instance mapIsElement :: IsElement HTML.HTMLMapElement
instance mediaIsElement :: IsElement HTML.HTMLMediaElement
instance metaIsElement :: IsElement HTML.HTMLMetaElement
instance meterIsElement :: IsElement HTML.HTMLMeterElement
instance modIsElement :: IsElement HTML.HTMLModElement
instance oListIsElement :: IsElement HTML.HTMLOListElement
instance objectIsElement :: IsElement HTML.HTMLObjectElement
instance optGroupIsElement :: IsElement HTML.HTMLOptGroupElement
instance optionIsElement :: IsElement HTML.HTMLOptionElement
instance outputIsElement :: IsElement HTML.HTMLOutputElement
instance paragraphIsElement :: IsElement HTML.HTMLParagraphElement
instance paramIsElement :: IsElement HTML.HTMLParamElement
instance preIsElement :: IsElement HTML.HTMLPreElement
instance progressIsElement :: IsElement HTML.HTMLProgressElement
instance quoteIsElement :: IsElement HTML.HTMLQuoteElement
instance scriptIsElement :: IsElement HTML.HTMLScriptElement
instance selectIsElement :: IsElement HTML.HTMLSelectElement
instance sourceIsElement :: IsElement HTML.HTMLSourceElement
instance spanIsElement :: IsElement HTML.HTMLSpanElement
instance styleIsElement :: IsElement HTML.HTMLStyleElement
instance tableCaptionIsElement :: IsElement HTML.HTMLTableCaptionElement
instance tableCellIsElement :: IsElement HTML.HTMLTableCellElement
instance tableColIsElement :: IsElement HTML.HTMLTableColElement
instance tableDataCellIsElement :: IsElement HTML.HTMLTableDataCellElement
instance tableIsElement :: IsElement HTML.HTMLTableElement
instance tableHeaderCellIsElement :: IsElement HTML.HTMLTableHeaderCellElement
instance tableRowIsElement :: IsElement HTML.HTMLTableRowElement
instance tableSectionIsElement :: IsElement HTML.HTMLTableSectionElement
instance templateIsElement :: IsElement HTML.HTMLTemplateElement
instance textAreaIsElement :: IsElement HTML.HTMLTextAreaElement
instance timeIsElement :: IsElement HTML.HTMLTimeElement
instance titleIsElement :: IsElement HTML.HTMLTitleElement
instance trackIsElement :: IsElement HTML.HTMLTrackElement
instance uListIsElement :: IsElement HTML.HTMLUListElement
instance videoIsElement :: IsElement HTML.HTMLVideoElement
