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
import Simple.JSON (readImpl)

foreign import request :: Response -> Effect Request
foreign import url :: Response -> Effect String
foreign import status :: Response -> Effect Int
foreign import statusText :: Response -> Effect String

foreign import _bodyBuffer :: Response -> Promise Buffer
foreign import _bodyJson :: Response -> Promise Foreign
foreign import _bodyText :: Response -> Promise String

foreign import _remoteAddressIp :: Response -> Effect Foreign
foreign import _remoteAddressPort :: Response -> Effect Foreign

bodyBuffer :: Response -> Aff Buffer
bodyBuffer = Promise.toAff <<< _bodyBuffer

bodyJson :: Response -> Aff Foreign
bodyJson = Promise.toAff <<< _bodyJson

bodyText :: Response -> Aff String
bodyText = Promise.toAff <<< _bodyText

remoteAddressIp :: Response -> Effect (Maybe String)
remoteAddressIp = map (hush <<< runExcept) <<< map readImpl <<< _remoteAddressIp

remoteAddressPort :: Response -> Effect (Maybe Int)
remoteAddressPort = map (hush <<< runExcept) <<< map readImpl <<< _remoteAddressPort
