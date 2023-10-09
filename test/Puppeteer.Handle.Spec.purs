module Puppeteer.Handle.Spec where

import Prelude

import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Either (hush)
import Data.Filterable (filterMap)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Newtype (wrap)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Buffer as Buffer
import Node.URL as Node.URL
import Puppeteer (timeout')
import Puppeteer as Pup
import Puppeteer.Browser as Pup.Browser
import Puppeteer.Eval as Pup.Eval
import Puppeteer.Handle as Pup.Handle
import Puppeteer.Handle.HTML as Pup.Handle.HTML
import Puppeteer.Page as Pup.Page
import Puppeteer.Page.Event as Pup.Page.Event
import Puppeteer.Page.Event.ConsoleMessage as ConsoleMessage
import Puppeteer.Selector as S
import Test.Spec (SpecT, aroundWith, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (failOnPageError, test)

html :: String
html =
  """
<html>
  <head>
    <title>Handle test</title>
    <script>
      document.addEventListener(
        'DOMContentLoaded',
        () => {
          const selectme = document.querySelector('#selectme')
          const clickme = document.querySelector('#clickme')
          const dragme = document.querySelector('#dragme')
          const dropme = document.querySelector('#dropme')

          selectme.addEventListener('change', e => {
            console.log('select(' + e.target.value + ')')
          })

          clickme.addEventListener('click', () => {
            console.log('clicked!')
          })

          dragme.addEventListener('drag', () => {
            console.log('drag')
          })

          dragme.addEventListener('dragend', () => {
            console.log('dragend')
          })

          dropme.addEventListener('dragenter', e => {
            e.preventDefault()
            console.log('dragenter')
          })

          dropme.addEventListener('dragover', e => {
            e.preventDefault()
            console.log('dragover')
          })
        },
      )
    </script>
  </head>
  <body>
    <div id="foo">
      <input type="text" id="input-a"></input>
      <input type="text" id="input-b"></input>
      <input type="file" id="input-c"></input>
      <select id="selectme">
        <option value="foo">foo</option>
        <option value="bar">bar</option>
        <option value="baz">baz</option>
      </select>
      <button id="clickme">click me!</button>
      <div id="bar">
        <span class="text">Hello, world!</span>
        <i style="visibility: hidden;" id="111abc222" name="frankie hobarth" cheeses="muenster cheddar brie"></i>
      </div>
      <div style="height: 100px; width: 100px; border: black solid 1px;" id="dragme" draggable="true"></div>
      <div style="height: 100px; width: 100px; border: red solid 1px;" id="dropme"></div>
      <div id="gone" style="position: fixed; top: 0; left: -100px; width: 10px; height: 10px;"></div>
      <i id="attrs" visible disabled class="fart"></i>
      <b></b>
      <a href="http://foo.com"></a>
      <a href="https://bar.com"></a>
      <a href="https://baz.com"></a>
      <a></a>
    </div>
  </body>
</html>
"""

withPage :: SpecT Aff Pup.Page Effect Unit -> SpecT Aff Unit Effect Unit
withPage =
  let
    withPage' spec' _ = do
      pup <- Pup.new
      b <- Pup.launch_ pup
      page <- Pup.Page.new b
      failOnPageError page do
        -- Pup.Page.Event.connectPageConsole page
        Pup.Page.setContent html Pup.Load page
        spec' page
      Pup.Browser.close b
  in
    aroundWith withPage'

spec :: SpecT Aff Unit Effect Unit
spec = withPage $ describe "Handle" do
  test "findAll" \p -> do
    body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
    divs <- Pup.Handle.findAll S.div body
    Array.length divs `shouldEqual` 5

  let
    findFirstOrHtmlEquals p = do
      body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
      foo <- liftMaybe (error "div#foo not found!") =<< Pup.Handle.findFirst (S.div `S.hasId` "foo") body
      firstDiv <- liftMaybe (error "no divs not found!") =<< Pup.Handle.findFirst S.div body
      shouldEqual true =<< Pup.Handle.HTML.equals foo firstDiv
      let
        i =
          S.i
            `S.hasAttrStartsWith` ("id" /\ "111")
            `S.hasAttrEndsWith` ("id" /\ "222")
            `S.hasAttrContaining` ("id" /\ "abc")
            `S.hasAttrListContaining` ("cheeses" /\ "muenster")
      void $ liftMaybe (error "i not found!") =<< Pup.Handle.findFirst i body

  test "findFirst" findFirstOrHtmlEquals

  let
    clickOrTap f p =
      do
        log <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Console p
        button <- liftMaybe (error "button#clickme not found!") =<< Pup.Page.findFirst (S.button `S.hasId` "clickme") p
        f button
        log' <- timeout' (wrap 100.0) $ joinFiber log
        ConsoleMessage.text log' `shouldEqual` "clicked!"

  test "click" $ clickOrTap Pup.Handle.click
  test "tap" $ clickOrTap Pup.Handle.tap

  test "clone" \p -> do
    aHandle <- Pup.Eval.unsafeRunJsHandle0 "() => ({a: 1, b: 'foo', c: ['a']})" p
    a <- Pup.Handle.clone aHandle
    a `shouldEqual` { a: 1, b: "foo", c: [ "a" ] }

  test "hover" \p -> do
    body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
    shouldEqual false <=< map isJust <<< Pup.Handle.findFirst (S.hover S.button) $ body
    btn <- liftMaybe (error "input not found!") =<< Pup.Handle.findFirst S.button body
    Pup.Handle.hover btn
    shouldEqual true <=< map isJust <<< Pup.Handle.findFirst (S.hover S.button) $ body

  test "isHidden" \p -> do
    body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
    shouldEqual false <=< map isJust <<< Pup.Handle.findFirst (S.hover S.button) $ body
    i <- liftMaybe (error "input not found!") =<< Pup.Handle.findFirst S.i body
    shouldEqual true =<< Pup.Handle.isHidden i

  test "isVisible" \p -> do
    body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
    shouldEqual false <=< map isJust <<< Pup.Handle.findFirst (S.hover S.button) $ body
    i <- liftMaybe (error "input not found!") =<< Pup.Handle.findFirst S.i body
    div <- liftMaybe (error "input not found!") =<< Pup.Handle.findFirst S.div body
    shouldEqual false =<< Pup.Handle.isVisible i
    shouldEqual true =<< Pup.Handle.isVisible div

  test "isIntersectingViewport" \p -> do
    body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
    shouldEqual false <=< map isJust <<< Pup.Handle.findFirst (S.hover S.button) $ body
    foo <- liftMaybe (error "input not found!") =<< Pup.Handle.findFirst (S.div `S.hasId` "foo") body
    gone <- liftMaybe (error "input not found!") =<< Pup.Handle.findFirst (S.div `S.hasId` "gone") body
    shouldEqual true =<< Pup.Handle.isIntersectingViewport foo
    shouldEqual false =<< Pup.Handle.isIntersectingViewport gone

  test "dragToElement" \p -> do
    body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
    dragme <- liftMaybe (error "dragme not found!") =<< Pup.Handle.findFirst (S.div `S.hasId` "dragme") body
    dropme <- liftMaybe (error "dropme not found!") =<< Pup.Handle.findFirst (S.div `S.hasId` "dropme") body
    let
      collectLogs as = do
        log <- Pup.Page.Event.once Pup.Page.Event.Console p
        let as' = as <> [ log ]
        pure $ if Array.length as' == 4 then Done as' else Loop as'
    logs <- forkAff $ tailRecM collectLogs []
    Pup.Handle.drop dragme dropme
    logs' <- timeout' (wrap 1000.0) $ joinFiber logs
    (ConsoleMessage.text <$> logs') `shouldEqual` [ "drag", "dragenter", "dragover", "dragend" ]

  test "screenshot" \p -> do
    body <- liftMaybe (error "body not found!") =<< Pup.Page.findFirst S.body p
    buf <- Pup.Handle.screenshot Pup.defaultScreenshot body
    void $ liftEffect $ Buffer.size buf

  test "select" \p -> do
    sel <- liftMaybe (error "select not found!") =<< Pup.Page.findFirst S.select p
    log <- forkAff $ Pup.Page.Event.once Pup.Page.Event.Console p
    Pup.Handle.select [ "foo" ] sel
    log' <- timeout' (wrap 1000.0) $ joinFiber log
    ConsoleMessage.text log' `shouldEqual` "select(foo)"

  test "getProperties" \p -> do
    o <- Pup.Eval.unsafeRunJsHandle0 "() => ({foo: 'foo', bar: 'bar', baz: 'baz'})" p
    props <- Pup.Handle.getProperties o
    Map.keys props `shouldEqual` Set.fromFoldable [ "foo", "bar", "baz" ]

  describe "HTML" do
    test "equals" findFirstOrHtmlEquals
    test "attrs" \p -> do
      anchors <- Pup.Page.findAll (S.anchor `S.hasAttr` "href") p
      hrefs <- filterMap (Map.lookup "href") <$> for anchors Pup.Handle.HTML.attrs
      urls <- liftEffect $ filterMap hush <$> for hrefs (try <<< Node.URL.new)
      Array.length urls `shouldEqual` 3

      anchors' <- Pup.Page.findAll (S.anchor `S.hasAttr` "href" `S.hasAttr` "disabled") p
      hrefs' <- filterMap (Map.lookup "href") <$> for anchors' Pup.Handle.HTML.attrs
      urls' <- liftEffect $ filterMap hush <$> for hrefs' (try <<< Node.URL.new)
      Array.length urls' `shouldEqual` 0
      pure unit
