module Puppeteer.HTTP.Response (request, url, status, statusText, bodyBuffer, bodyJson, bodyText, remoteAddressIp, remoteAddressPort) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Node.Buffer (Buffer)
import Puppeteer.Base (Request, Response)
import Puppeteer.FFI as FFI
import Simple.JSON (readImpl)

foreign import request :: Response -> Effect Request
foreign import url :: Response -> Effect String
foreign import status :: Response -> Effect Int
foreign import statusText :: Response -> Effect String

foreign import _bodyBuffer :: Response -> Effect (Promise Buffer)
foreign import _bodyJson :: Response -> Effect (Promise Foreign)
foreign import _bodyText :: Response -> Effect (Promise String)

foreign import _remoteAddressIp :: Response -> Effect Foreign
foreign import _remoteAddressPort :: Response -> Effect Foreign

bodyBuffer :: Response -> Aff Buffer
bodyBuffer = FFI.promiseToAff <<< _bodyBuffer

bodyJson :: Response -> Aff Foreign
bodyJson = FFI.promiseToAff <<< _bodyJson

bodyText :: Response -> Aff String
bodyText = FFI.promiseToAff <<< _bodyText

remoteAddressIp :: Response -> Effect (Maybe String)
remoteAddressIp = map (hush <<< runExcept) <<< map readImpl <<< _remoteAddressIp

remoteAddressPort :: Response -> Effect (Maybe Int)
remoteAddressPort = map (hush <<< runExcept) <<< map readImpl <<< _remoteAddressPort
