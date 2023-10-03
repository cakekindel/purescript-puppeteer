module Puppeteer.Plugin.Spec where

import Prelude

import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.ST as ST
import Control.Monad.ST.Global as ST
import Control.Monad.ST.Ref as ST
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error)
import Node.EventEmitter as EventEmitter
import Node.Process as Process
import Puppeteer as Pup
import Puppeteer.Eval as Pup.Eval
import Puppeteer.Page as Pup.Page
import Puppeteer.Page.Navigate as Pup.Page.Nav
import Puppeteer.Page.WaitFor as Pup.Page.WaitFor
import Puppeteer.Plugin.AdBlock as Pup.AdBlock
import Puppeteer.Plugin.AnonymousUserAgent as Pup.AnonUA
import Puppeteer.Plugin.Captcha as Pup.Captcha
import Puppeteer.Plugin.Stealth as Pup.Stealth
import Test.Spec (SpecT(..), describe, focus, pending)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Util (test)

spec :: SpecT Aff Unit Effect Unit
spec = describe "Plugin" do
  args <- liftEffect Process.argv

  let
    pendingUnlessArg a t b =
      if not $ Array.any (_ == a) args then do
        let msg = " (skipped unless `" <> a <> "`, ex. `spago test " <> a <> "`)"
        pending (t <> msg)
      else
        test t b

  describe "Captcha" do
    test "install" do
      pup <- Pup.new
      pup' <- liftEffect $ Pup.Captcha.install (Pup.Captcha.defaultOptions $ wrap "") pup
      void $ Pup.launch_ pup'

    pendingUnlessArg "--test-captcha" "solves captchas" do
      token <- liftMaybe (error "TWOCAPTCHA_API_KEY not present") <=< liftEffect <<< Process.lookupEnv $ "TWOCAPTCHA_API_KEY"
      let
        urls =
          [ "https://www.google.com/recaptcha/api2/demo"
          , "https://accounts.hcaptcha.com/demo"
          , "https://democaptcha.com/demo-form-eng/hcaptcha.html"
          ]
      pup <- Pup.new
      pup' <- liftEffect $ Pup.Captcha.install (Pup.Captcha.defaultOptions $ wrap token) pup
      b <- Pup.launch_ pup'
      sequential $ for_ urls \u -> parallel do
        p <- Pup.Page.new b
        _ <- Pup.Page.Nav.to_ p u
        { solved } <- Pup.Captcha.solveCaptchas pup' p
        Array.length solved `shouldSatisfy` (_ >= 1)
      pure unit
  describe "Adblock" do
    test "install" do
      pup <- Pup.new
      pup' <- liftEffect $ Pup.AdBlock.install Pup.AdBlock.defaultOptions pup
      void $ Pup.AdBlock.blocker pup'
    pendingUnlessArg "--test-adblock" "blocks ads" do
      pup <- Pup.new
      pup' <- liftEffect $ Pup.AdBlock.install Pup.AdBlock.defaultOptions pup
      blocker <- Pup.AdBlock.blocker pup'
      requestsBlocked <- liftEffect $ ST.toEffect (ST.new 0)
      stylesInjected <- liftEffect $ ST.toEffect (ST.new 0)
      let add1On st h = liftEffect $ EventEmitter.on_ h (void $ ST.toEffect $ ST.modify (_ + 1) st) blocker
      add1On requestsBlocked Pup.AdBlock.requestBlockedH
      add1On stylesInjected Pup.AdBlock.styleInjectedH
      b <- Pup.launch_ pup'
      p <- Pup.Page.new b
      _ <- Pup.Page.Nav.to_ p "https://www.google.com/search?q=rent%20a%20car"
      Pup.Page.WaitFor.networkIdle (Pup.Page.WaitFor.NetworkIdleFor $ wrap 200.0) p
      reqs <- liftEffect $ ST.toEffect $ ST.read requestsBlocked
      stys <- liftEffect $ ST.toEffect $ ST.read stylesInjected
      reqs `shouldSatisfy` (_ >= 1)
      stys `shouldSatisfy` (_ >= 1)
  describe "Stealth" do
    test "install" do
      pup <- Pup.new
      void $ liftEffect $ Pup.Stealth.install pup
  describe "AnonymousUserAgent" do
    test "install" do
      pup <- Pup.new
      void $ liftEffect $ Pup.AnonUA.install pup
