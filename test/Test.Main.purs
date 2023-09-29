module Test.Main where

import Prelude

import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Foldable (findMap, fold, foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect, foreachE)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Exception as Error
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Node.Stream as Writable
import Puppeteer.Browser.Spec as Spec.Browser
import Puppeteer.Page.Spec as Spec.Page
import Puppeteer.Spec as Spec
import Test.Spec (SpecT)
import Test.Spec.Config (defaultConfig)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (runSpecT)

foreign import errorString :: Error -> Effect String

main :: Effect Unit
main = launchAff_ do
  let cfg = defaultConfig { timeout = Nothing, exit = false }
  run <- liftEffect $ runSpecT cfg [ consoleReporter ] Spec.spec
  res <- (map (join <<< map (foldl Array.snoc [])) run) :: Aff (Array Result)
  let
    getError = case _ of
      Failure e -> Just e
      _ -> Nothing
  let errs = filterMap getError res
  liftEffect $ foreachE errs \e -> do
    _ <- Writable.writeString Process.stdout UTF8 $ Error.message e
    _ <- Writable.writeString Process.stdout UTF8 "\n"
    _ <- Writable.writeString Process.stdout UTF8 $ fromMaybe "" $ Error.stack e
    _ <- Writable.writeString Process.stdout UTF8 "\n"
    pure unit

  liftEffect $ Process.exit
