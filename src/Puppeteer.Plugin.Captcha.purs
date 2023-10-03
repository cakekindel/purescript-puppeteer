module Puppeteer.Plugin.Captcha
  ( install
  , findCaptchas
  , solveCaptchas
  , defaultOptions
  , CaptchaCallback(..)
  , Options
  , CaptchaProvider(..)
  , CaptchaVendor(..)
  , CaptchaPlugin
  , CaptchaKind(..)
  , CaptchaFiltered(..)
  , Token2Captcha(..)
  , CaptchaInfo
  , CaptchaInfoMaybeFiltered
  , CaptchaSolution
  , CaptchaSolved
  , CaptchaInfoDisplay
  , SolveResult
  , getSolutions
  , enterSolutions
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bifunctor (lmap)
import Data.Either (Either, hush)
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error, error)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, unsafeFromForeign, unsafeReadTagged, unsafeToForeign)
import Puppeteer.Base (JsDuplex(..), Page, Puppeteer, duplex, duplexRead, duplexWrite)
import Puppeteer.FFI as FFI
import Record (modify, rename)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Type.Prelude (Proxy(..))

newtype CoerceDate = CoerceDate (Maybe JSDate)

derive instance Newtype CoerceDate _

instance ReadForeign CoerceDate where
  readImpl f = pure $ CoerceDate $ hush $ runExcept $ unsafeReadTagged "Date" f

instance WriteForeign CoerceDate where
  writeImpl (CoerceDate f) = unsafeToForeign f

newtype Token2Captcha = Token2Captcha String

derive instance Newtype Token2Captcha _
derive instance Generic Token2Captcha _
instance Show Token2Captcha where
  show = genericShow

data CaptchaKind = KindCheckbox | KindInvisible | KindScore | KindOther String

derive instance Generic CaptchaKind _
derive instance Eq CaptchaKind
instance Show CaptchaKind where
  show = genericShow

instance WriteForeign CaptchaKind where
  writeImpl = writeImpl <<< case _ of
    KindCheckbox -> "checkbox"
    KindInvisible -> "invisible"
    KindScore -> "score"
    KindOther s -> s

instance ReadForeign CaptchaKind where
  readImpl =
    let
      fromStr = case _ of
        "checkbox" -> KindCheckbox
        "invisible" -> KindInvisible
        "score" -> KindScore
        s -> KindOther s
    in
      map fromStr <<< readImpl

data CaptchaVendor = VendorReCaptcha | VendorHCaptcha | VendorOther String

derive instance Generic CaptchaVendor _
derive instance Eq CaptchaVendor
instance Show CaptchaVendor where
  show = genericShow

vendorFromString :: String -> CaptchaVendor
vendorFromString = case _ of
  "recaptcha" -> VendorReCaptcha
  "hcaptcha" -> VendorHCaptcha
  s -> VendorOther s

instance ReadForeign CaptchaVendor where
  readImpl f = vendorFromString <$> readImpl f

instance WriteForeign CaptchaVendor where
  writeImpl VendorHCaptcha = writeImpl "hcaptcha"
  writeImpl VendorReCaptcha = writeImpl "recaptcha"
  writeImpl (VendorOther s) = writeImpl s

data CaptchaFiltered = FilteredScoreBased | FilteredNotInViewport | FilteredInactive

derive instance Generic CaptchaFiltered _
derive instance Eq CaptchaFiltered
instance Show CaptchaFiltered where
  show = genericShow

newtype CaptchaCallback = CaptchaCallback Foreign

derive instance Newtype CaptchaCallback _
derive newtype instance WriteForeign CaptchaCallback
derive newtype instance ReadForeign CaptchaCallback
derive instance Generic CaptchaCallback _
instance Show CaptchaCallback where
  show _ = "CaptchaCallback"

filteredFromString :: String -> Maybe CaptchaFiltered
filteredFromString = case _ of
  "solveInViewportOnly" -> Just FilteredNotInViewport
  "solveScoreBased" -> Just FilteredScoreBased
  "solveInactiveChallenges" -> Just FilteredInactive
  _ -> Nothing

type CaptchaInfoDisplay =
  { size :: Maybe Foreign
  , theme :: Maybe String
  , top :: Maybe Foreign
  , left :: Maybe Foreign
  , width :: Maybe Foreign
  , height :: Maybe Foreign
  }

type CaptchaInfoMaybeFiltered = Tuple CaptchaInfo (Maybe CaptchaFiltered)

type CaptchaSolution =
  { vendor :: Maybe CaptchaVendor
  , id :: Maybe String
  , text :: Maybe String
  , hasSolution :: Boolean
  , requestAt :: Maybe JSDate
  , responseAt :: Maybe JSDate
  , duration :: Maybe Number
  , provider :: Maybe String
  , providerCaptchaId :: Maybe String
  }

type CaptchaSolved =
  { vendor :: Maybe CaptchaVendor
  , id :: Maybe String
  , isSolved :: Maybe Boolean
  , responseElement :: Maybe Boolean
  , responseCallback :: Maybe Boolean
  , solvedAt :: Maybe JSDate
  }

duplexSolved :: JsDuplex CaptchaSolved _
duplexSolved =
  let
    toRaw r = modify (Proxy :: Proxy "solvedAt") CoerceDate
      $ r
    fromRaw r = pure
      $ modify (Proxy :: Proxy "solvedAt") unwrap
      $ r
  in
    duplex toRaw fromRaw

type SolveResult =
  { captchas :: Array CaptchaInfoMaybeFiltered
  , solved :: Array CaptchaSolved
  , solutions :: Array CaptchaSolution
  }

data CaptchaProvider
  = Provider2Captcha Token2Captcha
  | ProviderCustom (Array CaptchaInfo -> Aff (Array CaptchaSolution))

prepareCustomProvider :: (Array CaptchaInfo -> Aff (Array CaptchaSolution)) -> Array CaptchaInfo -> Promise { solutions :: Array CaptchaSolution }
prepareCustomProvider f = unsafePerformEffect <<< Promise.fromAff <<< map (\solutions -> { solutions }) <<< f

type Options =
  { visualize :: Maybe Boolean
  , skipNotInViewport :: Maybe Boolean
  , skipScoreBased :: Maybe Boolean
  , skipInactive :: Maybe Boolean
  , provider :: CaptchaProvider
  }

defaultOptions :: Token2Captcha -> Options
defaultOptions token = { visualize: Nothing, skipNotInViewport: Nothing, skipInactive: Nothing, skipScoreBased: Nothing, provider: Provider2Captcha token }

prepareOptions :: Options -> Foreign
prepareOptions { provider, visualize, skipInactive, skipNotInViewport, skipScoreBased } =
  writeImpl
    { provider: case provider of
        Provider2Captcha (Token2Captcha t) -> writeImpl { id: "2captcha", token: t }
        ProviderCustom f -> writeImpl { fn: unsafeToForeign $ prepareCustomProvider f }
    , visualFeedback: FFI.maybeToUndefined visualize
    , solveInViewportOnly: FFI.maybeToUndefined $ skipNotInViewport
    , solveScoreBased: FFI.maybeToUndefined $ not <$> skipScoreBased
    , solveInactiveChallenges: FFI.maybeToUndefined $ not <$> skipInactive
    , throwOnError: true
    }

type CaptchaInfo =
  { kind :: Maybe CaptchaKind
  , vendor :: Maybe CaptchaVendor
  , id :: Maybe String
  , sitekey :: Maybe String
  , s :: Maybe String
  , isInViewport :: Maybe Boolean
  , isInvisible :: Maybe Boolean
  , hasActiveChallengePopup :: Maybe Boolean
  , hasChallengeFrame :: Maybe Boolean
  , action :: Maybe String
  , callback :: CaptchaCallback
  , hasResponseElement :: Maybe Boolean
  , url :: Maybe String
  , display :: Maybe CaptchaInfoDisplay
  }

duplexSoln :: JsDuplex CaptchaSolution _
duplexSoln =
  let
    toRaw r = modify (Proxy :: Proxy "requestAt") CoerceDate
      $ modify (Proxy :: Proxy "responseAt") CoerceDate
      $ r
    fromRaw r = pure
      $ modify (Proxy :: Proxy "requestAt") (unwrap)
      $ modify (Proxy :: Proxy "responseAt") (unwrap)
      $ r
  in
    duplex toRaw fromRaw

duplexInfo :: JsDuplex CaptchaInfo _
duplexInfo =
  let
    toRaw r = rename (Proxy :: Proxy "kind") (Proxy :: Proxy "_type") $ r
    fromRaw r = pure $ rename (Proxy :: Proxy "_type") (Proxy :: Proxy "kind") r
  in
    duplex toRaw fromRaw

foreign import data CaptchaPlugin :: Type

foreign import _captcha :: forall (r :: Row Type). Foreign -> Puppeteer r -> Effect (Puppeteer (captcha :: CaptchaPlugin | r))
foreign import _findCaptchas :: Page -> Promise Foreign
foreign import _getSolutions :: Page -> Foreign -> Promise Foreign
foreign import _enterSolutions :: Page -> Foreign -> Promise Foreign
foreign import _solveCaptchas :: Page -> Promise Foreign

read :: forall @a. ReadForeign a => Foreign -> Either Error a
read = lmap (error <<< show) <<< runExcept <<< readImpl

install :: forall (r :: Row Type). Options -> Puppeteer r -> Effect (Puppeteer (captcha :: CaptchaPlugin | r))
install o p = _captcha (prepareOptions o) p

infos :: Foreign -> Either Error (Array CaptchaInfoMaybeFiltered)
infos f = do
  { captchas, filtered } <- read @({ captchas :: Array Foreign, filtered :: Array Foreign }) f
  captchas' <- sequence $ duplexRead duplexInfo <$> captchas
  let captchas'' = (_ /\ Nothing) <$> captchas'
  filtered' <- for filtered \f' -> do
    c <- duplexRead duplexInfo f'
    { filtered: wasF, filteredReason } <- read @({ filtered :: Boolean, filteredReason :: String }) f'
    pure $ case filteredFromString filteredReason of
      Just r | wasF -> c /\ (Just r)
      _ -> c /\ Nothing
  pure $ captchas'' <> filtered'

findCaptchas :: forall (r :: Row Type). Puppeteer (captcha :: CaptchaPlugin | r) -> Page -> Aff (Array CaptchaInfoMaybeFiltered)
findCaptchas _ p = do
  f <- Promise.toAff $ _findCaptchas p
  liftEither $ infos f

getSolutions :: forall (r :: Row Type). Puppeteer (captcha :: CaptchaPlugin | r) -> Page -> Array CaptchaInfo -> Aff (Array CaptchaSolution)
getSolutions _ p is = do
  f <- Promise.toAff $ _getSolutions p (writeImpl $ duplexWrite duplexInfo <$> is)
  { solutions } <- liftEither $ read @({ solutions :: Array Foreign }) f
  liftEither $ for solutions $ duplexRead duplexSoln

enterSolutions :: forall (r :: Row Type). Puppeteer (captcha :: CaptchaPlugin | r) -> Page -> Array CaptchaSolution -> Aff (Array CaptchaSolved)
enterSolutions _ p sols = do
  f <- Promise.toAff $ _enterSolutions p (writeImpl $ duplexWrite duplexSoln <$> sols)
  { solved } <- liftEither $ read @({ solved :: Array Foreign }) f
  liftEither $ for solved $ duplexRead duplexSolved

solveCaptchas :: forall (r :: Row Type). Puppeteer (captcha :: CaptchaPlugin | r) -> Page -> Aff SolveResult
solveCaptchas _ p = do
  f <- Promise.toAff $ _solveCaptchas p
  { solved, solutions } <- liftEither $ read @({ solved :: Array Foreign, solutions :: Array Foreign }) f
  captchas <- liftEither $ infos f
  liftEither do
    solved' <- for solved $ duplexRead duplexSolved
    solutions' <- for solutions $ duplexRead duplexSoln
    pure $ { captchas, solved: solved', solutions: solutions' }
