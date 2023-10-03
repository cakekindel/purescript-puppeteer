module Puppeteer.Plugin.Stealth where

import Effect (Effect)
import Puppeteer.Base (Puppeteer)

-- | https://github.com/berstend/puppeteer-extra/tree/master/packages/puppeteer-extra-plugin-stealth
foreign import data StealthPlugin :: Type
foreign import install :: forall (r :: Row Type). Puppeteer r -> Effect (Puppeteer (stealth :: StealthPlugin | r))
