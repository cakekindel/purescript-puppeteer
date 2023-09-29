module Puppeteer.Eval
  ( unsafeRunJs0
  , unsafeRunJsHandle0
  , unsafeRunJs1
  , unsafeRunJsHandle1
  , unsafeRunJs2
  , unsafeRunJsHandle2
  , unsafeRunJs3
  , unsafeRunJsHandle3
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Puppeteer.Base (class EvalTarget, Handle)

foreign import _run :: forall @r. String -> Foreign -> Array Foreign -> Promise r
foreign import _runh :: forall @r. String -> Foreign -> Array Foreign -> Promise (Handle r)

unsafeRunJs0 :: forall e @r. EvalTarget e => String -> e -> Aff r
unsafeRunJs0 js h = Promise.toAff $ _run js (unsafeToForeign h) []

unsafeRunJsHandle0 :: forall e @r. EvalTarget e => String -> e -> Aff (Handle r)
unsafeRunJsHandle0 js h = Promise.toAff $ _runh js (unsafeToForeign h) []

unsafeRunJs1 :: forall a e @r. EvalTarget e => String -> a -> e -> Aff r
unsafeRunJs1 js a h = Promise.toAff $ _run js (unsafeToForeign h) [ unsafeToForeign a ]

unsafeRunJsHandle1 :: forall a e @r. EvalTarget e => String -> a -> e -> Aff (Handle r)
unsafeRunJsHandle1 js a h = Promise.toAff $ _runh js (unsafeToForeign h) [ unsafeToForeign a ]

unsafeRunJs2 :: forall a b e @r. EvalTarget e => String -> a -> b -> e -> Aff r
unsafeRunJs2 js a b h = Promise.toAff $ _run js (unsafeToForeign h) [ unsafeToForeign a, unsafeToForeign b ]

unsafeRunJsHandle2 :: forall a b e @r. EvalTarget e => String -> a -> b -> e -> Aff (Handle r)
unsafeRunJsHandle2 js a b h = Promise.toAff $ _runh js (unsafeToForeign h) [ unsafeToForeign a, unsafeToForeign b ]

unsafeRunJs3 :: forall a b c e @r. EvalTarget e => String -> a -> b -> c -> e -> Aff r
unsafeRunJs3 js a b c h = Promise.toAff $ _run js (unsafeToForeign h) [ unsafeToForeign a, unsafeToForeign b, unsafeToForeign c ]

unsafeRunJsHandle3 :: forall a b c e @r. EvalTarget e => String -> a -> b -> c -> e -> Aff (Handle r)
unsafeRunJsHandle3 js a b c h = Promise.toAff $ _runh js (unsafeToForeign h) [ unsafeToForeign a, unsafeToForeign b, unsafeToForeign c ]
