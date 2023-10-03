module Puppeteer.Plugin.AnonymousUserAgent where

import Effect (Effect)
import Puppeteer.Base (Puppeteer)

-- | https://github.com/berstend/puppeteer-extra/tree/master/packages/puppeteer-extra-plugin-anonymize-ua
foreign import data AnonymousUserAgentPlugin :: Type
foreign import install :: forall (r :: Row Type). Puppeteer r -> Effect (Puppeteer (userAgent :: AnonymousUserAgentPlugin | r))
