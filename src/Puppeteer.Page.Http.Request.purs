module Puppeteer.Http.Request where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either, either, hush)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Node.Buffer (Buffer)
import Puppeteer.Base (Context(..), Request, Response)
import Puppeteer.FFI as FFI
import Puppeteer.Http (ErrorCode, errorCodeString)
import Simple.JSON (readImpl, writeImpl)

type RespondToRequest =
  { body :: Maybe (Either String Buffer)
  , contentType :: Maybe String
  , headers :: Maybe (Map String String)
  , status :: Maybe Int
  }

prepareRespondToRequest :: RespondToRequest -> Foreign
prepareRespondToRequest { body, contentType, headers: headers', status } = writeImpl
  { body: FFI.maybeToUndefined $ map (either unsafeToForeign unsafeToForeign) body
  , contentType: FFI.maybeToUndefined contentType
  , headers: FFI.maybeToUndefined $ map FFI.mapToRecord headers'
  , status: FFI.maybeToUndefined status
  }

type ContinueRequestOverrides =
  { headers :: Maybe (Map String String)
  , method :: Maybe String
  , postData :: Maybe String
  , url :: Maybe String
  }

prepareContinueRequestOverrides :: ContinueRequestOverrides -> Foreign
prepareContinueRequestOverrides { headers: headers', method: method', postData, url: url' } = writeImpl
  { headers: FFI.maybeToUndefined $ map FFI.mapToRecord headers'
  , method: FFI.maybeToUndefined method'
  , postData: FFI.maybeToUndefined postData
  , url: FFI.maybeToUndefined url'
  }

foreign import headers :: Request -> Effect (Map String String)
foreign import isNavigation :: Request -> Effect Boolean
foreign import method :: Request -> Effect String
foreign import resourceType :: Request -> Effect String
foreign import url :: Request -> Effect String

foreign import _abort :: String -> Request -> Promise Unit
foreign import _continue :: Foreign -> Request -> Promise Unit
foreign import _respond :: Foreign -> Request -> Promise Unit

foreign import _failure :: Request -> Effect (Nullable { errorText :: String })
foreign import _postData :: Request -> Effect Foreign
foreign import _response :: Request -> Effect (Nullable Response)

abort :: Context "intercepting requests" -> ErrorCode -> Request -> Aff Unit
abort _ e = Promise.toAff <<< _abort (errorCodeString e)

continue :: Context "intercepting requests" -> ContinueRequestOverrides -> Request -> Aff Unit
continue _ o = Promise.toAff <<< _continue (prepareContinueRequestOverrides o)

respond :: Context "intercepting requests" -> RespondToRequest -> Request -> Aff Unit
respond _ r = Promise.toAff <<< _respond (prepareRespondToRequest r)

failure :: Request -> Effect (Maybe String)
failure = map (map _.errorText <<< Nullable.toMaybe) <<< _failure

postData :: Request -> Effect (Maybe String)
postData = map (hush <<< runExcept <<< readImpl) <<< _postData

response :: Request -> Effect (Maybe Response)
response = map Nullable.toMaybe <<< _response
