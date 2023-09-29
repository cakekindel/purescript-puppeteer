module Puppeteer.Page.Navigate where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (LifecycleEvent(..), Page, URL, prepareLifecycleEvent)
import Puppeteer.HTTP as HTTP

foreign import _forward :: Foreign -> Page -> Promise (Maybe HTTP.Response)
foreign import _back :: Foreign -> Page -> Promise (Maybe HTTP.Response)
foreign import _reload :: Foreign -> Page -> Promise (Maybe HTTP.Response)
foreign import _to :: String -> Foreign -> Page -> Promise (Maybe HTTP.Response)

forward :: LifecycleEvent -> Page -> Aff (Maybe HTTP.Response)
forward ev = Promise.toAff <<< _forward (prepareLifecycleEvent ev)

back :: LifecycleEvent -> Page -> Aff (Maybe HTTP.Response)
back ev = Promise.toAff <<< _back (prepareLifecycleEvent ev)

to :: URL -> LifecycleEvent -> Page -> Aff (Maybe HTTP.Response)
to url ev = Promise.toAff <<< _to url (prepareLifecycleEvent ev)

reload :: LifecycleEvent -> Page -> Aff (Maybe HTTP.Response)
reload ev = Promise.toAff <<< _reload (prepareLifecycleEvent ev)

forward_ :: Page -> Aff (Maybe HTTP.Response)
forward_ = forward Load

back_ :: Page -> Aff (Maybe HTTP.Response)
back_ = back Load

to_ :: URL -> Page -> Aff (Maybe HTTP.Response)
to_ url = to url Load

reload_ :: Page -> Aff (Maybe HTTP.Response)
reload_ = reload Load
