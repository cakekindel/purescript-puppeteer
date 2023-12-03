module Puppeteer.Plugin.AdBlock
  ( AdBlockMode(..)
  , AdBlockOptions
  , AdBlockPlugin
  , AdBlocker
  , install
  , defaultOptions
  , blocker
  , cspInjectedH
  , htmlFilteredH
  , requestAllowedH
  , requestBlockedH
  , requestRedirectedH
  , requestWhitelistedH
  , scriptInjectedH
  , styleInjectedH
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Node.EventEmitter (EventEmitter)
import Node.EventEmitter as EventEmitter
import Node.EventEmitter.UtilTypes (EventHandle0) as EventEmitter
import Puppeteer.Base (Puppeteer)
import Puppeteer.FFI as FFI
import Simple.JSON (writeImpl)

-- | https://github.com/berstend/puppeteer-extra/tree/master/packages/puppeteer-extra-plugin-adblocker
foreign import data AdBlockPlugin :: Type
foreign import data AdBlocker :: Type

-- | https://github.com/berstend/puppeteer-extra/tree/master/packages/puppeteer-extra-plugin-adblocker#options
data AdBlockMode
  -- | Block ads, but not trackers
  = BlockAds
  -- | Block ads & trackers
  | BlockTrackers
  -- | Block ads, trackers & annoyances
  | BlockAnnoyances

type AdBlockOptions = { mode :: AdBlockMode, useDiskCache :: Boolean, cacheDir :: Maybe String }

defaultOptions :: AdBlockOptions
defaultOptions = { mode: BlockAds, useDiskCache: true, cacheDir: Nothing }

prepareOptions :: AdBlockOptions -> Foreign
prepareOptions { mode, useDiskCache, cacheDir } = FFI.mergeRecords
  [ writeImpl case mode of
      BlockAds -> { blockTrackers: false, blockTrackersAndAnnoyances: false }
      BlockTrackers -> { blockTrackers: true, blockTrackersAndAnnoyances: false }
      BlockAnnoyances -> { blockTrackers: true, blockTrackersAndAnnoyances: true }
  , writeImpl { useCache: useDiskCache, cacheDir: FFI.maybeToUndefined cacheDir }
  ]

foreign import _install :: forall (r :: Row Type). Foreign -> Puppeteer r -> Effect (Puppeteer (adblock :: AdBlockPlugin | r))
foreign import _blocker :: forall (r :: Row Type). Puppeteer r -> Effect (Promise AdBlocker)

install :: forall (r :: Row Type). AdBlockOptions -> Puppeteer r -> Effect (Puppeteer (adblock :: AdBlockPlugin | r))
install o p = _install (prepareOptions o) p

blocker :: forall (r :: Row Type). Puppeteer (adblock :: AdBlockPlugin | r) -> Aff AdBlocker
blocker = FFI.promiseToAff <<< _blocker

cspInjectedH :: EventEmitter.EventHandle0 AdBlocker
cspInjectedH = EventEmitter.EventHandle "csp-injected" identity

htmlFilteredH :: EventEmitter.EventHandle0 AdBlocker
htmlFilteredH = EventEmitter.EventHandle "html-filtered" identity

requestAllowedH :: EventEmitter.EventHandle0 AdBlocker
requestAllowedH = EventEmitter.EventHandle "request-allowed" identity

requestBlockedH :: EventEmitter.EventHandle0 AdBlocker
requestBlockedH = EventEmitter.EventHandle "request-blocked" identity

requestRedirectedH :: EventEmitter.EventHandle0 AdBlocker
requestRedirectedH = EventEmitter.EventHandle "request-redirected" identity

requestWhitelistedH :: EventEmitter.EventHandle0 AdBlocker
requestWhitelistedH = EventEmitter.EventHandle "request-whitelisted" identity

scriptInjectedH :: EventEmitter.EventHandle0 AdBlocker
scriptInjectedH = EventEmitter.EventHandle "script-injected" identity

styleInjectedH :: EventEmitter.EventHandle0 AdBlocker
styleInjectedH = EventEmitter.EventHandle "style-injected" identity
