module Puppeteer.Page.Event.Spec where

import Prelude

import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Control.Monad.Rec.Class (untilJust)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global as ST
import Control.Monad.ST.Ref as ST
import Control.Monad.ST.Ref as ST.Ref
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay, forkAff, joinFiber)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error)
import Effect.Exception as Error
import Puppeteer (timeout)
import Puppeteer as Pup
import Puppeteer.Base (timeout')
import Puppeteer.Browser as Pup.Browser
import Puppeteer.Eval as Pup.Eval
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

pageEmpty :: String
pageEmpty =
  """
<html>
<head></head>
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
  beforeAll (Pup.launch_ =<< Pup.new)
    $ afterAll Pup.Browser.close
    $ do
        describe "Event" do
          withPage $ test "listen PageError" \p -> do
            errorsST <- liftST $ ST.Ref.new []
            let handle = void <<< liftST <<< flip ST.Ref.modify errorsST <<< Array.cons
            listening <- liftEffect $ Pup.Page.Event.listen Pup.Page.Event.PageError handle p
            void $ Pup.Page.addScriptTag (Pup.Page.AddScriptInline scriptError) p
            err <- timeout' (wrap 1000.0) $ untilJust (liftST $ Array.head <$> ST.Ref.read errorsST)
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
            dialog <- timeout' (wrap 3000.0) $ joinFiber dialogF
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
            timeout' (wrap 1000.0) $ joinFiber requestIntercepted
            timeout' (wrap 1000.0) $ joinFiber loadEvent
            log' <- timeout' (wrap 1000.0) $ joinFiber log
            ConsoleMessage.text log' `shouldEqual` "hi"

          withPage $ test "DomContentLoaded, Load" \p -> failOnPageError p do
            continueST <- liftST $ ST.Ref.new false
            let
              rep = Pup.HTTP.Request.defaultRespond
                { body = Just (Left "console.log('hi')")
                , contentType = Just "text/javascript"
                }

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
            timeout' (wrap 100.0) $ joinFiber requestIntercepted
            timeout' (wrap 100.0) $ joinFiber f
            shouldBeLoaded true

          test "Close" \b -> do
            p <- Pup.Page.new b
            failOnPageError p do
              closeF <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Close p
              Pup.Page.close p
              joinFiber closeF

          test "exclusive" \b -> do
            msgST <- liftEffect $ ST.toEffect $ ST.new $ Right Nothing
            p <- Pup.Page.new b
            Pup.Page.setContent pageEmpty Pup.Load p
            onceLog <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Console p
            exclusive <- liftEffect $ Pup.Page.Event.exclusive
              Pup.Page.Event.Console
              ( \m -> do
                  prev <- ST.toEffect $ ST.modify (const $ Right Nothing) msgST
                  case prev of
                    Right (Just _) -> void $ ST.toEffect $ ST.write (Left $ error $ "last message was not taken") msgST
                    Right Nothing -> void $ ST.toEffect $ ST.write (Right $ Just m) msgST
                    Left _ -> pure unit
              )
              p

            Pup.Eval.unsafeRunJs0 @Unit "() => console.log('cheddar')" p
            delay $ wrap 50.0

            cheddarEM <- liftEffect $ ST.toEffect $ ST.read msgST
            void $ liftEffect $ ST.toEffect $ ST.write (Right Nothing) msgST
            cheddar <- liftMaybe (error "cheddar: listener did not fire") =<< liftEither cheddarEM
            (ConsoleMessage.text cheddar) `shouldEqual` "cheddar"

            Pup.Eval.unsafeRunJs0 @Unit "() => console.log('brie')" p
            brieEM <- liftEffect $ ST.toEffect $ ST.read msgST
            void $ liftEffect $ ST.toEffect $ ST.write (Right Nothing) msgST
            brie <- liftMaybe (error "brie: listener did not fire") =<< liftEither brieEM
            (ConsoleMessage.text brie) `shouldEqual` "brie"

            Pup.closeContext exclusive

            Pup.Eval.unsafeRunJs0 @Unit "() => console.log('camembert')" p
            camembertEM <- liftEffect $ ST.toEffect $ ST.read msgST
            void $ liftEffect $ ST.toEffect $ ST.write (Right Nothing) msgST
            maybe (pure unit) (const $ throwError $ error "camembert: listener wasn't removed") =<< liftEither camembertEM
            camembertFromOnce <- joinFiber onceLog
            (ConsoleMessage.text camembertFromOnce) `shouldEqual` "camembert"
