module Puppeteer.Mouse.Spec where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Puppeteer as Pup
import Puppeteer.Base (timeout')
import Puppeteer.Browser as Pup.Browser
import Puppeteer.Handle as Pup.Handle
import Puppeteer.Handle.HTML as Pup.Handle.HTML
import Puppeteer.Keyboard as Pup.Keyboard
import Puppeteer.Mouse as Mouse
import Puppeteer.Page as Pup.Page
import Puppeteer.Page.Event (connectPageConsole)
import Puppeteer.Page.Event.Spec as Spec.Page.Event
import Puppeteer.Page.WaitFor as Pup.Page.WaitFor
import Test.Spec (SpecT, afterAll, beforeAll, beforeWith, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (failOnPageError, test)

content :: String
content =
  """
<!DOCTYPE html>
<html>
  <head>
    <title>Simple Page</title>
    <script>
      window.addEventListener('load', () => {
        document.querySelector('#foo').addEventListener('mouseenter', () => {
          const div = document.createElement('div')
          div.classList.add('foo-mouseenter')
          document.body.append(div)
        })
  
        document.querySelector('#bar').addEventListener(
          'mouseenter',
          () => {
            const div = document.createElement('div')
            div.classList.add('bar-mouseenter')
            document.body.append(div)
          },
        )
  
        document.querySelector('#foo').addEventListener('wheel', () => {
          const div = document.createElement('div')
          div.classList.add('foo-wheel')
          document.body.append(div)
        })
  
        document.querySelector('#bar').addEventListener('wheel', () => {
          const div = document.createElement('div')
          div.classList.add('bar-wheel')
          document.body.append(div)
        })
  
        document.querySelector('#foo').addEventListener('mousedown', () => {
          const div = document.createElement('div')
          div.classList.add('foo-mousedown')
          document.body.append(div)
        })
  
        document.querySelector('#bar').addEventListener('mousedown', () => {
          const div = document.createElement('div')
          div.classList.add('bar-mousedown')
          document.body.append(div)
        })

        document.querySelector('#foo').addEventListener('mouseup', () => {
          const div = document.createElement('div')
          div.classList.add('foo-mouseup')
          document.body.append(div)
        })
  
        document.querySelector('#bar').addEventListener('mouseup', () => {
          const div = document.createElement('div')
          div.classList.add('bar-mouseup')
          document.body.append(div)
        })
      })
    </script>
  </head>
  <body>
    <div id="foo" style="width: 100px; height: 100px; position: fixed; top: 200px; left: 100px;"></div>
    <div id="bar" style="width: 100px; height: 100px; position: fixed; left: 100px;"></div>
  </body>
</html>
"""

spec :: SpecT Aff Unit Effect Unit
spec = beforeAll (Pup.launch_ =<< Pup.new)
  $ afterAll Pup.Browser.close
  $ describe "Mouse" do
      test "move" \b -> do
        p <- Pup.Page.new b

        liftEffect $ connectPageConsole p
        failOnPageError p do
          Pup.Page.setContent content Pup.Load p
          mouse <- liftEffect $ Pup.Page.mouse p
          { x: fooX, y: fooY } <- liftMaybe (error "#foo does not have bounding box")
            =<< Pup.Handle.boundingBox
            =<< liftMaybe (error "#foo not found")
            =<< Pup.Page.findFirst "div#foo" p
          { x: barX, y: barY } <- liftMaybe (error "#bar does not have bounding box")
            =<< Pup.Handle.boundingBox
            =<< liftMaybe (error "#bar not found")
            =<< Pup.Page.findFirst "div#bar" p

          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseenter" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseenter" p

          Mouse.moveTo { steps: 10.0 } mouse { x: fooX + 1.0, y: fooY + 1.0 }
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseenter" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseenter" p

          Mouse.moveTo { steps: 10.0 } mouse { x: barX + 1.0, y: barY + 1.0 }
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseenter" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseenter" p

      test "up / down" \b -> do
        p <- Pup.Page.new b

        liftEffect $ connectPageConsole p
        failOnPageError p do
          Pup.Page.setContent content Pup.Load p
          mouse <- liftEffect $ Pup.Page.mouse p
          { x: fooX, y: fooY } <- liftMaybe (error "#foo does not have bounding box")
            =<< Pup.Handle.boundingBox
            =<< liftMaybe (error "#foo not found")
            =<< Pup.Page.findFirst "div#foo" p
          { x: barX, y: barY } <- liftMaybe (error "#bar does not have bounding box")
            =<< Pup.Handle.boundingBox
            =<< liftMaybe (error "#bar not found")
            =<< Pup.Page.findFirst "div#bar" p

          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p

          Mouse.moveTo { steps: 10.0 } mouse { x: fooX + 1.0, y: fooY + 1.0 }
          Mouse.down Mouse.MouseLeft mouse
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p

          Mouse.up Mouse.MouseLeft mouse
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p

          Mouse.moveTo { steps: 10.0 } mouse { x: barX + 1.0, y: barY + 1.0 }
          Mouse.down Mouse.MouseLeft mouse
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p

          Mouse.up Mouse.MouseLeft mouse
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p

      test "click" \b -> do
        p <- Pup.Page.new b

        liftEffect $ connectPageConsole p
        failOnPageError p do
          Pup.Page.setContent content Pup.Load p
          mouse <- liftEffect $ Pup.Page.mouse p
          { x: fooX, y: fooY } <- liftMaybe (error "#foo does not have bounding box")
            =<< Pup.Handle.boundingBox
            =<< liftMaybe (error "#foo not found")
            =<< Pup.Page.findFirst "div#foo" p
          { x: barX, y: barY } <- liftMaybe (error "#bar does not have bounding box")
            =<< Pup.Handle.boundingBox
            =<< liftMaybe (error "#bar not found")
            =<< Pup.Page.findFirst "div#bar" p

          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p

          Mouse.click {} mouse { x: fooX + 1.0, y: fooY + 1.0 }
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 0 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p

          Mouse.click {} mouse { x: barX + 1.0, y: barY + 1.0 }
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mousedown" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.foo-mouseup" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.bar-mousedown" p
          shouldEqual 1 =<< Array.length <$> Pup.Page.findAll "div.bar-mouseup" p
