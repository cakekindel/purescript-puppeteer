module Puppeteer.Browser.Spec where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Puppeteer as Pup
import Puppeteer.Browser as Pup.Browser
import Test.Spec (SpecT, afterAll, beforeAll, describe)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Util (test, testE)

spec :: SpecT Aff Unit Effect Unit
spec = beforeAll (Pup.launch_ =<< Pup.puppeteer unit)
  $ describe "Browser" do
      testE "websocketEndpoint" $ shouldNotEqual "" <=< Pup.Browser.websocketEndpoint
      testE "connected" $ shouldEqual true <=< Pup.Browser.connected
      test "disconnect and close" $ \b -> do
        ws <- liftEffect $ Pup.Browser.websocketEndpoint b
        liftEffect $ Pup.Browser.disconnect b
        connected <- liftEffect $ Pup.Browser.connected b
        connected `shouldEqual` false

        pup <- Pup.puppeteer unit
        b' <- Pup.connect (Pup.connectDefault $ Pup.BrowserWebsocket ws) pup
        Pup.Browser.close b'
