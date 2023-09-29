module Puppeteer.Page.Event.Spec where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Rec.Class (untilJust)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global as ST
import Control.Monad.ST.Ref as ST.Ref
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Exception as Error
import Puppeteer as Pup
import Puppeteer.Base (timeoutThrow)
import Puppeteer.Handle as Pup.Handle
import Puppeteer.Handle.HTML as Pup.Handle.HTML
import Puppeteer.Keyboard as Pup.Keyboard
import Puppeteer.Page as Pup.Page
import Puppeteer.Page.Event (connectPageConsole)
import Puppeteer.Page.Event as Pup.Page.Event
import Puppeteer.Page.Event.ConsoleMessage as ConsoleMessage
import Puppeteer.Page.WaitFor as Pup.Page.WaitFor
import Test.Spec (SpecT, afterAll, beforeAll, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (failOnPageError, test)

scriptError :: String
scriptError = "throw new Error('eek!')"

scriptLog :: String
scriptLog = "console.log('beak')"

listenIntoSTArray :: forall e ed. Pup.Page.Event.Event e ed => e -> Pup.Page -> Aff ({ st :: ST.Ref.STRef ST.Global (Array ed), cleanup :: Aff Unit })
listenIntoSTArray e p = do
  st <- liftST $ ST.Ref.new []
  let
    handle ed = do
      eds <- liftST $ ST.Ref.read st
      _ <- liftST $ ST.Ref.write (eds <> [ ed ]) st
      pure unit
  t <- Pup.Page.Event.listen e handle p
  pure { st, cleanup: Pup.closeContext t }

spec :: SpecT Aff Unit Effect Unit
spec =
  beforeAll (Pup.Page.new =<< Pup.launch_ =<< Pup.puppeteer unit)
    $ afterAll Pup.Page.close
    $ describe "Page" do
        test "listen, PageError" \p -> do
          { st: errsST, cleanup } <- listenIntoSTArray Pup.Page.Event.PageError p
          _ <- Pup.Page.addScriptTag (Pup.Page.AddScriptInline Pup.Page.Script scriptError) p
          err <- timeoutThrow (wrap 1000.0)
            $ untilJust do
                errs <- liftST $ ST.Ref.read errsST
                pure $ Array.head errs
          Error.message err `shouldEqual` "eek!"
          cleanup

        test "once" \p -> do
          errF <- forkAff $ Pup.Page.Event.once Pup.Page.Event.PageError p
          _ <- Pup.Page.addScriptTag (Pup.Page.AddScriptInline Pup.Page.Script scriptError) p
          err <- joinFiber errF
          Error.message err `shouldEqual` "eek!"

        test "Console" \p -> do
          logF <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Console p
          _ <- Pup.Page.addScriptTag (Pup.Page.AddScriptInline Pup.Page.Script scriptLog) p
          log <- joinFiber logF
          ConsoleMessage.text log `shouldEqual` "beak"
          ConsoleMessage.messageType log `shouldEqual` ConsoleMessage.Log
