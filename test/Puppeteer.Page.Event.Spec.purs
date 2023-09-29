module Puppeteer.Page.Event.Spec where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Ref as ST.Ref
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber)
import Effect.Exception as Error
import Puppeteer (timeout)
import Puppeteer as Pup
import Puppeteer.Base (timeoutThrow)
import Puppeteer.Browser as Pup.Browser
import Puppeteer.HTTP.Request as Pup.HTTP.Request
import Puppeteer.Page as Pup.Page
import Puppeteer.Page.Event as Pup.Page.Event
import Puppeteer.Page.Event.ConsoleMessage as ConsoleMessage
import Puppeteer.Page.Event.Dialog as Dialog
import Puppeteer.Page.HTTP as Pup.Page.HTTP
import Test.Spec (SpecT, afterAll, aroundWith, beforeAll, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (failOnPageError, test)

scriptError :: String
scriptError = "throw new Error('eek!')"

pageRequestsJs :: String
pageRequestsJs =
  """
<html>
<head>
  <script defer src="http://remote.org/index.js"></script>
</head>
<body></body>
</html>
"""

scriptUnblocks :: String
scriptUnblocks = "window.unblock = true"

scriptDialog :: String
scriptDialog = "alert('wow!')"

scriptLog :: String
scriptLog = "console.log('beak')"

withPage :: SpecT Aff Pup.Page Effect Unit -> SpecT Aff Pup.Browser Effect Unit
withPage =
  let
    withPage' spec' b = do
      page <- Pup.Page.new b
      spec' page
      Pup.Page.close page
  in
    aroundWith withPage'

spec :: SpecT Aff Unit Effect Unit
spec =
  beforeAll (Pup.launch_ =<< Pup.puppeteer unit)
    $ afterAll Pup.Browser.close
    $ do
        describe "Event" do
          withPage $ test "listen PageError" \p -> do
            errorsST <- liftST $ ST.Ref.new []
            let handle = void <<< liftST <<< flip ST.Ref.modify errorsST <<< Array.cons
            listening <- Pup.Page.Event.listen Pup.Page.Event.PageError handle p
            void $ Pup.Page.addScriptTag (Pup.Page.AddScriptInline scriptError) p
            err <- timeoutThrow (wrap 1000.0) $ untilJust (liftST $ Array.head <$> ST.Ref.read errorsST)
            Error.message err `shouldEqual` "eek!"
            Pup.closeContext listening

          withPage $ test "once" \p -> do
            errF <- forkAff $ Pup.Page.Event.once Pup.Page.Event.PageError p
            _ <- Pup.Page.addScriptTag (Pup.Page.AddScriptInline scriptError) p
            err <- joinFiber errF
            Error.message err `shouldEqual` "eek!"

          withPage $ test "Console" \p -> failOnPageError p do
            logF <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Console p
            _ <- Pup.Page.addScriptTag (Pup.Page.AddScriptInline scriptLog) p
            log <- joinFiber logF
            ConsoleMessage.text log `shouldEqual` "beak"
            ConsoleMessage.messageType log `shouldEqual` ConsoleMessage.Log

          withPage $ test "Dialog" \p -> failOnPageError p do
            dialogF <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Dialog p
            script <- forkAff $ Pup.Page.addScriptTag (Pup.Page.AddScriptInline scriptDialog) p
            dialog <- timeoutThrow (wrap 3000.0) $ joinFiber dialogF
            Dialog.dismiss dialog
            void $ joinFiber script

          withPage $ test "Request" \p -> failOnPageError p do
            let
              rep = Pup.HTTP.Request.defaultRespond
                { body = Just (Left "console.log('hi')")
                , contentType = Just "text/javascript"
                }
            let onrequest c = Pup.HTTP.Request.respond c rep
            requestIntercepted <- forkAff $ Pup.Page.HTTP.interceptNextRequest onrequest p
            log <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Console p
            loadEvent <- forkAff $ Pup.Page.setContent pageRequestsJs Pup.Load p
            timeoutThrow (wrap 1000.0) $ joinFiber requestIntercepted
            timeoutThrow (wrap 1000.0) $ joinFiber loadEvent
            log' <- timeoutThrow (wrap 1000.0) $ joinFiber log
            ConsoleMessage.text log' `shouldEqual` "hi"

          withPage $ test "DomContentLoaded, Load" \p -> failOnPageError p do
            continueST <- liftST $ ST.Ref.new false
            let
              rep = Pup.HTTP.Request.defaultRespond
                { body = Just (Left "console.log('hi')")
                , contentType = Just "text/javascript"
                }
            let
              onrequest c r = do
                untilJust do
                  continue <- liftST $ ST.Ref.read continueST
                  if not continue then delay $ wrap 100.0 else pure unit
                  pure $ if continue then Just unit else Nothing
                Pup.HTTP.Request.respond c rep r
            requestIntercepted <- forkAff $ Pup.Page.HTTP.interceptNextRequest onrequest p
            f <- forkAff $ Pup.Page.setContent pageRequestsJs Pup.Load p
            domContentLoaded <- forkAff $ Pup.Page.Event.once Pup.Page.Event.DomContentLoaded p
            loaded <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Load p
            let loaded' = timeout (wrap 100.0) $ joinFiber domContentLoaded <$ joinFiber loaded
            let shouldBeLoaded yn = shouldEqual yn =<< map isJust loaded'
            shouldBeLoaded false
            _ <- liftST $ ST.Ref.write true continueST
            timeoutThrow (wrap 100.0) $ joinFiber requestIntercepted
            timeoutThrow (wrap 100.0) $ joinFiber f
            shouldBeLoaded true

          test "Close" \b -> do
            p <- Pup.Page.new b
            failOnPageError p do
              closeF <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Close p
              Pup.Page.close p
              joinFiber closeF
