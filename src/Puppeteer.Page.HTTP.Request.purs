module Puppeteer.HTTP.Request
  ( ContinueRequestOverrides
  , RespondToRequest
  , defaultContinue
  , defaultRespond
  , respond
  , continue
  , abort
  , failure
  , postData
  , response
  , headers
  , isNavigation
  , method
  , resourceType
  , url
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either, either, hush)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Node.Buffer (Buffer)
import Puppeteer.Base (Context(..), Request, Response)
import Puppeteer.FFI as FFI
import Puppeteer.HTTP (ErrorCode, errorCodeString)
import Puppeteer.Page.HTTP (InterceptRequestsHint)
import Simple.JSON (readImpl, writeImpl)

type RespondToRequest =
  { body :: Maybe (Either String Buffer)
  , contentType :: Maybe String
  , headers :: Maybe (Map String String)
  , status :: Maybe Int
  }

defaultRespond :: RespondToRequest
defaultRespond = { body: Nothing, contentType: Nothing, headers: Nothing, status: Nothing }

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

defaultContinue :: ContinueRequestOverrides
defaultContinue = { url: Nothing, postData: Nothing, headers: Nothing, method: Nothing }

prepareContinueRequestOverrides :: ContinueRequestOverrides -> Foreign
prepareContinueRequestOverrides { headers: headers', method: method', postData: postData', url: url' } = writeImpl
  { headers: FFI.maybeToUndefined $ map FFI.mapToRecord headers'
  , method: FFI.maybeToUndefined method'
  , postData: FFI.maybeToUndefined postData'
  , url: FFI.maybeToUndefined url'
  }

foreign import isNavigation :: Request -> Effect Boolean
foreign import method :: Request -> Effect String
foreign import resourceType :: Request -> Effect String
foreign import url :: Request -> Effect String

foreign import _abort :: String -> Request -> Effect (Promise Unit)
foreign import _continue :: Foreign -> Request -> Effect (Promise Unit)
foreign import _respond :: Foreign -> Request -> Effect (Promise Unit)

foreign import _headers :: Request -> Effect (Array { k :: String, v :: String })
foreign import _failure :: Request -> Effect (Nullable { errorText :: String })
foreign import _postData :: Request -> Effect Foreign
foreign import _response :: Request -> Effect (Nullable Response)

headers :: Request -> Effect (Map String String)
headers = map FFI.makeMap <<< _headers

abort :: Context InterceptRequestsHint -> ErrorCode -> Request -> Aff Unit
abort _ e = Promise.toAffE <<< _abort (errorCodeString e)

continue :: Context InterceptRequestsHint -> ContinueRequestOverrides -> Request -> Aff Unit
continue _ o = Promise.toAffE <<< _continue (prepareContinueRequestOverrides o)

respond :: Context InterceptRequestsHint -> RespondToRequest -> Request -> Aff Unit
respond _ r = Promise.toAffE <<< _respond (prepareRespondToRequest r)

failure :: Request -> Effect (Maybe String)
failure = map (map _.errorText <<< Nullable.toMaybe) <<< _failure

postData :: Request -> Effect (Maybe String)
postData = map (hush <<< runExcept <<< readImpl) <<< _postData

response :: Request -> Effect (Maybe Response)
response = map Nullable.toMaybe <<< _response
