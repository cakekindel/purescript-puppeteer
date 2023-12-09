module Puppeteer.FFI (mapToRecord, maybeToUndefined, mergeRecords, unsafeMaybeToUndefined, makeMap, promiseToAff) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (any)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Data.String.Utils (includes) as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Exception as Error
import Foreign (Foreign, unsafeReadTagged)
import Simple.JSON (class WriteForeign)

foreign import mergeRecords :: Array Foreign -> Foreign
foreign import anyToString :: Foreign -> String
foreign import _mapToRecord :: forall a. Array { k :: String, v :: a } -> Foreign
foreign import _maybeToUndefined :: forall a. (Maybe a -> Nullable a) -> Maybe a -> Foreign

makeMap :: forall k v. Ord k => Array { k :: k, v :: v } -> Map k v
makeMap = Map.fromFoldable <<< map (\{ k, v } -> Tuple k v)

mapToRecord :: forall a. WriteForeign a => Map String a -> Foreign
mapToRecord = _mapToRecord <<< foldlWithIndex (\k a v -> Array.cons { k, v } a) []

maybeToUndefined :: forall a. WriteForeign a => Maybe a -> Foreign
maybeToUndefined = _maybeToUndefined Nullable.toNullable

unsafeMaybeToUndefined :: forall a. Maybe a -> Foreign
unsafeMaybeToUndefined = _maybeToUndefined Nullable.toNullable

promiseToAff :: forall a. Effect (Promise a) -> Aff a
promiseToAff work =
  let
    err e = either (const $ error $ anyToString e) identity
      $ runExcept
      $ unsafeReadTagged "Error" e

    retryErrorsMatching =
      [ "execution context destroyed"
      ]

    shouldRetry e =
      any
        (\retryErr -> String.includes retryErr $ String.toLower $ String.trim $ Error.message e)
        retryErrorsMatching

    attempt = do
      promise <- liftEffect work
      Promise.toAff' err promise

    retry e =
      if shouldRetry e then do
        promise <- liftEffect work
        Promise.toAff' err promise
      else
        throwError e
  in
    catchError attempt retry
