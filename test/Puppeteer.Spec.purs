module Puppeteer.Spec where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Puppeteer as Pup
import Puppeteer.Browser as Pup.Browser
import Test.Spec (SpecT, describe, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (test)

spec :: SpecT Aff Unit Effect Unit
spec = describe "Puppeteer" do
  test "launch" do
    pup <- Pup.puppeteer unit
    map void Pup.launch_ pup

  test "connect" do
    pup <- Pup.puppeteer unit

    b1 <- Pup.launch_ pup
    ws <- liftEffect $ Pup.Browser.websocketEndpoint b1

    liftEffect do
      shouldEqual true <=< Pup.Browser.connected $ b1
      Pup.Browser.disconnect b1
      shouldEqual false <=< Pup.Browser.connected $ b1

    b2 <- Pup.connect (Pup.connectDefault $ Pup.BrowserWebsocket ws) pup
    Pup.Browser.close b2
