module Puppeteer.Http (module X, ErrorCode(..), ResourceType(..), errorCodeString, resourceTypeOfString) where

import Puppeteer.Base (Request, Response) as X

data ErrorCode
  = Aborted
  | AccessDenied
  | AddressUnreachable
  | BlockedByClient
  | BlockedByResponse
  | ConnectionAborted
  | ConnectionClosed
  | ConnectionFailed
  | ConnectionRefused
  | ConnectionReset
  | InternetDisconnected
  | NameNotResolved
  | TimedOut
  | Failed

data ResourceType
  = Document
  | Stylesheet
  | Image
  | Media
  | Font
  | Script
  | TextTrack
  | XHR
  | Fetch
  | Prefetch
  | EventSource
  | WebSocket
  | Manifest
  | SignedExchange
  | Ping
  | CSPViolationReport
  | Preflight
  | Other

resourceTypeOfString :: String -> ResourceType
resourceTypeOfString "document" = Document
resourceTypeOfString "stylesheet" = Stylesheet
resourceTypeOfString "image" = Image
resourceTypeOfString "media" = Media
resourceTypeOfString "font" = Font
resourceTypeOfString "script" = Script
resourceTypeOfString "texttrack" = TextTrack
resourceTypeOfString "xhr" = XHR
resourceTypeOfString "fetch" = Fetch
resourceTypeOfString "prefetch" = Prefetch
resourceTypeOfString "eventsource" = EventSource
resourceTypeOfString "websocket" = WebSocket
resourceTypeOfString "manifest" = Manifest
resourceTypeOfString "signedexchange" = SignedExchange
resourceTypeOfString "ping" = Ping
resourceTypeOfString "cspviolationreport" = CSPViolationReport
resourceTypeOfString "preflight" = Preflight
resourceTypeOfString _ = Other

errorCodeString :: ErrorCode -> String
errorCodeString Aborted = "aborted"
errorCodeString AccessDenied = "accessdenied"
errorCodeString AddressUnreachable = "addressunreachable"
errorCodeString BlockedByClient = "blockedbyclient"
errorCodeString BlockedByResponse = "blockedbyresponse"
errorCodeString ConnectionAborted = "connectionaborted"
errorCodeString ConnectionClosed = "connectionclosed"
errorCodeString ConnectionFailed = "connectionfailed"
errorCodeString ConnectionRefused = "connectionrefused"
errorCodeString ConnectionReset = "connectionreset"
errorCodeString InternetDisconnected = "internetdisconnected"
errorCodeString NameNotResolved = "namenotresolved"
errorCodeString TimedOut = "timedout"
errorCodeString Failed = "failed"
