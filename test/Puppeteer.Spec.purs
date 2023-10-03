module Puppeteer.Spec where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Puppeteer as Pup
import Puppeteer.Browser as Pup.Browser
import Puppeteer.Browser.Spec as Spec.Browser
import Puppeteer.Handle.Spec as Spec.Handle
import Puppeteer.Page.Spec as Spec.Page
import Puppeteer.Plugin.Spec as Spec.Plugin
import Puppeteer.Selector.Spec as Spec.Selector
import Test.Spec (SpecT, describe, mapSpecTree)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (test)

spec :: SpecT Aff Unit Effect Unit
spec = describe "Puppeteer" do
  test "launch" do
    pup <- Pup.new
    map void Pup.launch_ pup

  test "connect" do
    pup <- Pup.new

    b1 <- Pup.launch_ pup
    ws <- liftEffect $ Pup.Browser.websocketEndpoint b1

    liftEffect do
      shouldEqual true <=< Pup.Browser.connected $ b1
      Pup.Browser.disconnect b1
      shouldEqual false <=< Pup.Browser.connected $ b1

    b2 <- Pup.connect (Pup.connectDefault $ Pup.BrowserWebsocket ws) pup
    Pup.Browser.close b2

  Spec.Browser.spec
  Spec.Page.spec
  Spec.Handle.spec
  Spec.Plugin.spec
  mapSpecTree (pure <<< unwrap) identity Spec.Selector.spec
