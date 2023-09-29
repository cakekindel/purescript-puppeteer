module Puppeteer.Page.Spec where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Puppeteer as Pup
import Puppeteer.Base (timeoutThrow)
import Puppeteer.Handle as Pup.Handle
import Puppeteer.Handle.HTML as Pup.Handle.HTML
import Puppeteer.Page as Pup.Page
import Puppeteer.Keyboard as Pup.Keyboard
import Puppeteer.Page.Event (connectPageConsole)
import Puppeteer.Page.WaitFor as Pup.Page.WaitFor
import Test.Spec (SpecT, beforeAll, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (failOnPageError, test)

styleFoo :: String
styleFoo =
  """
#foo {
  width: 200px;
}
"""

scriptAddBar :: String
scriptAddBar =
  """
const bar = document.createElement('div')
bar.id = 'bar'
document.body.append(bar)
"""

simplePage :: String
simplePage =
  """
<!DOCTYPE html>
<html>
  <head>
    <title>Simple Page</title>
  </head>
  <body>
    <div id="foo">
      <p>Foo</p>
      <p>Bar</p>
      <p>Baz</p>
    </div>
    <div></div>
  </body>
</html>
"""

inputPage :: String
inputPage =
  """
<!DOCTYPE html>
<html>
  <head>
    <title>Input Page</title>
  </head>
  <body>
    <input type="text"></input>
  </body>
</html>
"""

spec :: SpecT Aff Unit Effect Unit
spec = beforeAll (Pup.launch_ =<< Pup.puppeteer unit)
  $ describe "Page" do
      test "new, close, isClosed" \b -> do
        p <- Pup.Page.new b
        let shouldBeClosed b' = shouldEqual b' <=< liftEffect <<< Pup.Page.isClosed $ p
        shouldBeClosed false
        Pup.Page.close p
        shouldBeClosed true

      test "all" \b -> do
        let pageCountShouldBe n = shouldEqual n <=< map Array.length <<< Pup.Page.all $ b
        pageCountShouldBe 1
        p <- Pup.Page.new b
        pageCountShouldBe 2
        Pup.Page.close p
        pageCountShouldBe 1

      test "bringToFront" \b -> do
        p <- Pup.Page.new b
        Pup.Page.bringToFront p
        Pup.Page.close p

      test "setContent, content" \b -> do
        p <- Pup.Page.new b
        let html = "<html><head></head><body></body></html>"
        Pup.Page.setContent html Pup.Load p
        c <- Pup.Page.content p
        c `shouldEqual` html
        Pup.Page.close p

      test "setViewport, viewport" \b -> do
        p <- Pup.Page.new b
        let
          vp =
            { deviceScaleFactor: Nothing
            , hasTouch: Nothing
            , height: 1200
            , width: 800
            , isLandscape: Nothing
            , isMobile: Nothing
            }

        Pup.Page.setViewport vp p
        vp' <- liftMaybe (error "no viewport!") $ Pup.Page.viewport p
        vp' `shouldEqual` vp

      test "title" \b -> do
        p <- Pup.Page.new b
        Pup.Page.setContent simplePage Pup.Load p
        shouldEqual "Simple Page" =<< Pup.Page.title p

      test "url" \b -> do
        p <- Pup.Page.new b
        url <- liftEffect $ Pup.Page.url p
        url `shouldEqual` "about:blank"

      test "findAll" \b -> do
        p <- Pup.Page.new b
        Pup.Page.setContent simplePage Pup.Load p
        let selectorCountShouldBe s n = shouldEqual n =<< Array.length <$> Pup.Page.findAll s p
        selectorCountShouldBe "div" 2
        selectorCountShouldBe "div#foo" 1

      test "findFirst" \b -> do
        p <- Pup.Page.new b
        Pup.Page.setContent simplePage Pup.Load p
        let maybeNoDivs = liftMaybe (error "no divs!")
        div <- maybeNoDivs =<< Array.head <$> Pup.Page.findAll "div" p
        divFF <- maybeNoDivs =<< Pup.Page.findFirst "div" p
        isEq <- Pup.Handle.HTML.equals div divFF
        shouldEqual isEq true

      test "addStyleTag" \b -> do
        p <- Pup.Page.new b
        connectPageConsole p
        failOnPageError p do
          Pup.Page.setContent simplePage Pup.Load p
          _ <- Pup.Page.addStyleTag (Pup.Page.AddStyleInline styleFoo) p
          foo <- liftMaybe (error "#foo not found") =<< Pup.Page.findFirst "div#foo" p
          style <- Pup.Handle.HTML.computedStyle foo
          width <- liftMaybe (error "#foo doesn't have width") $ Map.lookup "width" style
          width `shouldEqual` "200px"
          Pup.Page.close p

      test "addScriptTag" \b -> do
        p <- Pup.Page.new b
        connectPageConsole p
        failOnPageError p do
          Pup.Page.setContent simplePage Pup.Load p
          _ <- Pup.Page.addScriptTag (Pup.Page.AddScriptInline Pup.Page.Script scriptAddBar) p
          _ <- timeoutThrow (Milliseconds 5000.0) $ Pup.Page.WaitFor.selector "div#bar" p
          Pup.Page.close p

      test "keyboard" \b -> do
        p <- Pup.Page.new b
        connectPageConsole p
        failOnPageError p do
          Pup.Page.setContent inputPage Pup.Load p
          input <- liftMaybe (error "no inputs!") =<< Pup.Page.findFirst "input" p
          input' <- liftMaybe (error "not an input!") =<< Pup.Handle.HTML.toHTMLInputElement input
          shouldEqual "" =<< Pup.Handle.HTML.value input'
          Pup.Handle.focus input
          kb <- liftEffect $ Pup.Page.keyboard p
          Pup.Keyboard.doType "foo bar bingus bat" kb
          shouldEqual "foo bar bingus bat" =<< Pup.Handle.HTML.value input'
          Pup.Page.close p
