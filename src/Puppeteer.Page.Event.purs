module Puppeteer.Page.Event
  ( once
  , listen
  , eventKey
  , eventData
  , class Event
  , connectPageConsole
  , defaultEventData
  , FrameEvent(..)
  , UnitEvent(..)
  , ErrorEvent(..)
  , NullablePageEvent(..)
  , RequestEvent(..)
  , ResponseEvent(..)
  , DialogEvent(..)
  , ConsoleMessageEvent(..)
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (hush, note)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, unsafeFromForeign)
import Puppeteer.Base (Context(..), Frame, Page)
import Puppeteer.HTTP as HTTP
import Puppeteer.Page as Page
import Puppeteer.Page.Event.ConsoleMessage (ConsoleMessage, messageTypeString)
import Puppeteer.Page.Event.ConsoleMessage as ConsoleMessage
import Puppeteer.Page.Event.Dialog (Dialog)
import Simple.JSON (class ReadForeign, readImpl)

connectPageConsole :: Page -> Effect Unit
connectPageConsole p =
  let
    onmsg m = launchAff_ do
      title <- Page.title p
      let t = ConsoleMessage.messageType m
      let textLevel = "[" <> String.toUpper (messageTypeString t) <> "]"
      let textPrefix = "[Puppeteer.Page@\"" <> title <> "\"]"
      let text = textLevel <> " " <> textPrefix <> " " <> ConsoleMessage.text m
      liftEffect $ case t of
        ConsoleMessage.Debug -> Console.debug text
        ConsoleMessage.Error -> Console.error text
        ConsoleMessage.Warning -> Console.warn text
        _ -> Console.log text
  in
    void $ listen Console onmsg p

data UnitEvent
  = Close
  | DomContentLoaded
  | Load

instance unitEvent :: Event UnitEvent Unit where
  eventKey Close = "close"
  eventKey DomContentLoaded = "domcontentloaded"
  eventKey Load = "load"
  eventData = const $ pure unit

data ErrorEvent
  = Error
  | PageError

instance errorEvent :: Event ErrorEvent Error where
  eventKey Error = "error"
  eventKey PageError = "pageerror"
  eventData = pure <<< unsafeFromForeign

data FrameEvent
  = FrameAttached
  | FrameDetached
  | FrameNavigated

instance frameEvent :: Event FrameEvent Frame where
  eventKey FrameAttached = "frameattached"
  eventKey FrameDetached = "framedetached"
  eventKey FrameNavigated = "framenavigated"
  eventData = defaultEventData

data ConsoleMessageEvent = Console

instance consoleEvent :: Event ConsoleMessageEvent ConsoleMessage where
  eventKey Console = "console"
  eventData = defaultEventData

data DialogEvent = Dialog

instance dialogEvent :: Event DialogEvent Dialog where
  eventKey Dialog = "dialog"
  eventData = defaultEventData

data NullablePageEvent = Popup

instance nullablePageEvent :: Event NullablePageEvent (Nullable Page) where
  eventKey Popup = "popup"
  eventData = defaultEventData

data ResponseEvent = Response

instance responseEvent :: Event ResponseEvent HTTP.Response where
  eventKey Response = "response"
  eventData = defaultEventData

data RequestEvent
  = Request
  | RequestFailed
  | RequestFinished
  | RequestServedFromCache

instance requestEvent :: Event RequestEvent HTTP.Request where
  eventKey Request = "request"
  eventKey RequestFailed = "requestfailed"
  eventKey RequestFinished = "requestfinished"
  eventKey RequestServedFromCache = "requestservedfromcache"
  eventData = defaultEventData

class Event :: Type -> Type -> Constraint
class Event ev d | ev -> d, d -> ev where
  eventKey :: ev -> String
  eventData :: Foreign -> Maybe d

defaultEventData :: forall d. ReadForeign d => Foreign -> Maybe d
defaultEventData = hush <<< runExcept <<< readImpl

foreign import data ListenerToken :: Type
foreign import _once :: String -> (Foreign -> Unit) -> Page -> Effect Unit
foreign import _addListener :: String -> (Foreign -> Unit) -> Page -> Effect ListenerToken
foreign import _removeListener :: ListenerToken -> Page -> Effect Unit

once :: forall ev evd. Event ev evd => ev -> Page -> Aff evd
once ev p =
  let
    k = eventKey ev
    f res = do
      let cb = unsafePerformEffect <<< res <<< lmap error <<< note "parse failed" <<< eventData
      _once k cb p
      pure mempty
  in
    makeAff f

listen :: forall ev evd. Event ev evd => ev -> (evd -> Effect Unit) -> Page -> Effect (Context "event listener")
listen ev cb p =
  let
    cb' f = unsafePerformEffect $ do
      evd <- liftEither $ lmap error $ note "parse failed" $ eventData f
      cb evd
  in
    do
      t <- _addListener (eventKey ev) cb' p
      pure $ Context (\_ -> liftEffect $ _removeListener t p)
