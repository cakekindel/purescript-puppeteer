module Test.Util where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither, try)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Puppeteer as Pup
import Puppeteer.Page.Event as Pup.Page.Event
import Test.Spec (class Example, SpecT, hoistSpec, it)

test_ :: forall m t arg g g'. Monad m => Example t arg g' => (g' ~> g) -> String -> t -> SpecT g arg m Unit
test_ gaff s t = hoistSpec identity (\_ -> gaff) $ it s t

test :: forall m t arg g. Monad m => Example t arg g => String -> t -> SpecT g arg m Unit
test = test_ identity

testE :: forall m t arg g. MonadEffect g => Monad m => Example t arg Effect => String -> t -> SpecT g arg m Unit
testE = test_ liftEffect

testA :: forall m t arg g. MonadAff g => Monad m => Example t arg Aff => String -> t -> SpecT g arg m Unit
testA = test_ liftAff

failOnPageError :: forall a. Pup.Page -> Aff a -> Aff a
failOnPageError p a =
  let
    ok = parallel $ try a
    err = parallel $ Left <$> Pup.Page.Event.once Pup.Page.Event.PageError p
  in
    liftEither =<< (sequential $ ok <|> err)
