module Puppeteer
  ( module X
  , new
  , connect
  , launch
  , connect_
  , launch_
  , launchNonHeadless
  , connectDefault
  , launchDefault
  , BrowserConnection(..)
  , Launch
  , Connect
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Puppeteer.Base (Puppeteer, duplexWrite)
import Puppeteer.Base as X
import Puppeteer.Browser (Browser)
import Puppeteer.Browser as Browser
import Puppeteer.FFI as FFI
import Puppeteer.Screenshot as X
import Simple.JSON (writeImpl)

--| [https://pptr.dev/api/puppeteer.puppeteerlaunchoptions]
type Launch =
  { product :: Maybe Browser.Product
  , extraPrefsFirefox :: Maybe (Map String String)
  , channel :: Maybe Browser.ChromeReleaseChannel
  , dumpio :: Maybe Boolean
  , env :: Maybe (Map String String)
  , handleSIGHUP :: Maybe Boolean
  , handleSIGINT :: Maybe Boolean
  , handleSIGTERM :: Maybe Boolean
  , args :: Maybe (Array String)
  , debuggingPort :: Maybe Int
  , devtools :: Maybe Boolean
  , headless :: Boolean
  , userDataDir :: Maybe String
  , browser :: Maybe Browser.Connect
  }

data BrowserConnection
  = BrowserURL String
  | BrowserWebsocket String

--| [https://pptr.dev/api/puppeteer.connectoptions]
type Connect =
  { connection :: BrowserConnection
  , headers :: Maybe (Map String String)
  , transport :: Maybe { close :: Maybe (Effect Unit), send :: Maybe (String -> Effect Unit) }
  , browser :: Maybe Browser.Connect
  }

connectDefault :: BrowserConnection -> Connect
connectDefault connection =
  { connection
  , headers: Nothing
  , transport: Nothing
  , browser: Nothing
  }

launchDefault :: Launch
launchDefault =
  { product: Nothing
  , extraPrefsFirefox: Nothing
  , channel: Nothing
  , dumpio: Nothing
  , env: Nothing
  , handleSIGHUP: Nothing
  , handleSIGINT: Nothing
  , handleSIGTERM: Nothing
  , args: Nothing
  , debuggingPort: Nothing
  , devtools: Nothing
  , headless: true
  , userDataDir: Nothing
  , browser: Nothing
  }

prepareConnectOptions :: Connect -> Foreign
prepareConnectOptions
  { connection
  , headers
  , transport
  , browser
  } =
  let
    transport' { close, send } =
      { close: FFI.unsafeMaybeToUndefined $ map (\close' _ -> unsafePerformEffect close') close
      , send: FFI.unsafeMaybeToUndefined $ map (\send' s -> unsafePerformEffect $ send' s) send
      }
    browserURL = case connection of
      BrowserURL u -> Just u
      _ -> Nothing
    browserWSEndpoint = case connection of
      BrowserWebsocket u -> Just u
      _ -> Nothing
  in
    FFI.mergeRecords
      [ writeImpl
          { browserURL: FFI.maybeToUndefined browserURL
          , browserWSEndpoint: FFI.maybeToUndefined browserWSEndpoint
          , headers: FFI.maybeToUndefined $ map FFI.mapToRecord headers
          , transport: FFI.maybeToUndefined $ map transport' transport
          }
      , writeImpl $ map (duplexWrite Browser.duplexConnect) browser
      ]

prepareLaunchOptions :: Launch -> Foreign
prepareLaunchOptions
  { product
  , extraPrefsFirefox
  , channel
  , dumpio
  , env
  , handleSIGHUP
  , handleSIGINT
  , handleSIGTERM
  , args
  , debuggingPort
  , devtools
  , headless
  , userDataDir
  , browser
  } =
  let
    product' Browser.Chrome = "chrome"
    product' Browser.Firefox = "firefox"
    channel' Browser.ChromeStable = "chrome"
    channel' Browser.ChromeBeta = "chrome-beta"
    channel' Browser.ChromeCanary = "chrome-canary"
    channel' Browser.ChromeDev = "chrome-dev"
  in
    FFI.mergeRecords
      [ writeImpl
          { product: FFI.maybeToUndefined $ map product' product
          , extraPrefsFirefox: FFI.maybeToUndefined $ map FFI.mapToRecord extraPrefsFirefox
          , env: FFI.maybeToUndefined $ map FFI.mapToRecord env
          , dumpio: FFI.maybeToUndefined dumpio
          , channel: FFI.maybeToUndefined $ map channel' channel
          , handleSIGHUP: FFI.maybeToUndefined handleSIGHUP
          , handleSIGINT: FFI.maybeToUndefined handleSIGINT
          , handleSIGTERM: FFI.maybeToUndefined handleSIGTERM
          , args: FFI.maybeToUndefined args
          , debuggingPort: FFI.maybeToUndefined debuggingPort
          , devtools: FFI.maybeToUndefined devtools
          , headless: if headless then writeImpl "new" else writeImpl false
          , userDataDir: FFI.maybeToUndefined userDataDir
          }
      , writeImpl $ FFI.maybeToUndefined $ map (duplexWrite Browser.duplexConnect) browser
      ]

foreign import _puppeteer :: Effect (Promise (Puppeteer ()))
foreign import _connect :: forall p. Foreign -> Puppeteer p -> Effect (Promise Browser)
foreign import _launch :: forall p. Foreign -> Puppeteer p -> Effect (Promise Browser)

--| Create a new puppeteer instance
--|
--| [`PuppeteerExtra`](https://github.com/berstend/puppeteer-extra/blob/master/packages/puppeteer-extra/src/index.ts)
--| [`PuppeteerNode`](https://pptr.dev/api/puppeteer.puppeteernode)
new :: Aff (Puppeteer ())
new = Promise.toAffE _puppeteer

--| Connect to an existing browser instance
--|
--| [`PuppeteerNode#connect`](https://pptr.dev/api/puppeteer.puppeteernode.connect)
connect :: forall p. Connect -> Puppeteer p -> Aff Browser
connect c = Promise.toAffE <<< _connect (prepareConnectOptions c)

connect_ :: forall p. Puppeteer p -> Aff Browser
connect_ = Promise.toAffE <<< _connect (prepareLaunchOptions launchDefault)

--| Launch a new browser instance
--|
--| [`PuppeteerNode#launch`](https://pptr.dev/api/puppeteer.puppeteernode.launch)
launch :: forall p. Launch -> Puppeteer p -> Aff Browser
launch l = Promise.toAffE <<< _launch (prepareLaunchOptions l)

launch_ :: forall p. Puppeteer p -> Aff Browser
launch_ = Promise.toAffE <<< _launch (prepareLaunchOptions launchDefault)

launchNonHeadless :: forall p. Puppeteer p -> Aff Browser
launchNonHeadless = Promise.toAffE <<< _launch (prepareLaunchOptions $ launchDefault { headless = false })
