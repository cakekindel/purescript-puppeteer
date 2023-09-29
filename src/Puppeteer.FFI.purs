module Puppeteer.FFI (mapToRecord, maybeToUndefined, mergeRecords, unsafeMaybeToUndefined, makeMap) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Simple.JSON (class WriteForeign)

foreign import mergeRecords :: Array Foreign -> Foreign
foreign import _mapToRecord :: forall a. Array {k :: String, v :: a} -> Foreign
foreign import _maybeToUndefined :: forall a. (Maybe a -> Nullable a) -> Maybe a -> Foreign

makeMap :: forall k v. Ord k => Array { k :: k, v :: v } -> Map k v
makeMap = Map.fromFoldable <<< map (\{ k, v } -> Tuple k v)

mapToRecord :: forall a. WriteForeign a => Map String a -> Foreign
mapToRecord = _mapToRecord <<< foldlWithIndex (\k a v -> Array.cons {k, v} a) []

maybeToUndefined :: forall a. WriteForeign a => Maybe a -> Foreign
maybeToUndefined = _maybeToUndefined Nullable.toNullable

unsafeMaybeToUndefined :: forall a. Maybe a -> Foreign
unsafeMaybeToUndefined = _maybeToUndefined Nullable.toNullable
