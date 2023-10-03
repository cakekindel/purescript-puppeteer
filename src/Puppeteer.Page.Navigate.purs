module Puppeteer.Page.Navigate where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (LifecycleEvent(..), Page, URL, duplexLifecycleEvent, duplexWrite)
import Puppeteer.HTTP as HTTP

foreign import _forward :: Foreign -> Page -> Effect (Promise (Maybe HTTP.Response))
foreign import _back :: Foreign -> Page -> Effect (Promise (Maybe HTTP.Response))
foreign import _reload :: Foreign -> Page -> Effect (Promise (Maybe HTTP.Response))
foreign import _to :: String -> Foreign -> Page -> Effect (Promise (Maybe HTTP.Response))

forward :: LifecycleEvent -> Page -> Aff (Maybe HTTP.Response)
forward ev = Promise.toAffE <<< _forward (duplexWrite duplexLifecycleEvent ev)

back :: LifecycleEvent -> Page -> Aff (Maybe HTTP.Response)
back ev = Promise.toAffE <<< _back (duplexWrite duplexLifecycleEvent ev)

to :: LifecycleEvent -> Page -> URL -> Aff (Maybe HTTP.Response)
to ev p u = Promise.toAffE $ _to u (duplexWrite duplexLifecycleEvent ev) p

reload :: LifecycleEvent -> Page -> Aff (Maybe HTTP.Response)
reload ev = Promise.toAffE <<< _reload (duplexWrite duplexLifecycleEvent ev)

forward_ :: Page -> Aff (Maybe HTTP.Response)
forward_ = forward Load

back_ :: Page -> Aff (Maybe HTTP.Response)
back_ = back Load

to_ :: Page -> URL -> Aff (Maybe HTTP.Response)
to_ = to Load

reload_ :: Page -> Aff (Maybe HTTP.Response)
reload_ = reload Load
