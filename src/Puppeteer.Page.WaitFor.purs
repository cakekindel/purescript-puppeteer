module Puppeteer.Page.WaitFor
  ( WaitingForNavigationHint
  , NetworkIdleFor(..)
  , navigation
  , networkIdle
  , selector
  , selectorToBeVisible
  , selectorToBeHidden
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (Context(..), Handle, LifecycleEvent, Page, prepareLifecycleEvent)
import Puppeteer.Selector (class Selector, toCSS)

newtype NetworkIdleFor = NetworkIdleFor Milliseconds

derive instance ntIdleFor :: Newtype NetworkIdleFor _

type WaitingForNavigationHint :: Symbol
type WaitingForNavigationHint = "`Puppeteer.Page.WaitFor.navigation` was initiated, waiting for your code to trigger the navigation. Invoke `Puppeteer.closeContext` to block once the navigation has been initiated."

foreign import _navigation :: Foreign -> Page -> Effect (Promise Unit)
foreign import _networkIdle :: Number -> Page -> Promise Unit
foreign import _selector :: forall a. String -> Page -> Promise (Handle a)
foreign import _selectorToExist :: forall a. String -> Page -> Promise (Handle a)
foreign import _selectorToBeHidden :: String -> Page -> Promise Unit

navigation :: LifecycleEvent -> Page -> Effect (Context WaitingForNavigationHint)
navigation ev p = do
  promise <- _navigation (prepareLifecycleEvent ev) p
  pure $ Context (\_ -> Promise.toAff $ promise)

networkIdle :: NetworkIdleFor -> Page -> Aff Unit
networkIdle i = Promise.toAff <<< _networkIdle (unwrap $ unwrap i)

selector :: forall s e. Selector s e => s -> Page -> Aff (Handle e)
selector s = Promise.toAff <<< _selectorToExist (toCSS s)

selectorToBeVisible :: forall s e. Selector s e => s -> Page -> Aff (Handle e)
selectorToBeVisible s = Promise.toAff <<< _selector (toCSS s)

selectorToBeHidden :: forall s e. Selector s e => s -> Page -> Aff Unit
selectorToBeHidden s = Promise.toAff <<< _selectorToBeHidden (toCSS s)
