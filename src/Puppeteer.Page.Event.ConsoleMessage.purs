module Puppeteer.Page.Event.ConsoleMessage
  ( MessageType(..)
  , messageType
  , ConsoleMessage
  , args
  , text
  , location
  , stackTrace
  , messageTypeString
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Foreign (Foreign, unsafeFromForeign)
import Puppeteer.Base (Handle)
import Puppeteer.FFI as FFI
import Simple.JSON (class ReadForeign, readImpl, writeImpl)

type Location = { columnNumber :: Maybe Int, lineNumber :: Maybe Int, url :: Maybe String }

locationFromJs :: Foreign -> Maybe Location
locationFromJs = hush <<< runExcept <<< readImpl

locationToJs :: Location -> Foreign
locationToJs { columnNumber, lineNumber, url } =
  writeImpl
    { columnNumber: FFI.maybeToUndefined columnNumber
    , lineNumber: FFI.maybeToUndefined lineNumber
    , url: FFI.maybeToUndefined url
    }

data MessageType
  = Log
  | Debug
  | Info
  | Error
  | Warning
  | Dir
  | Dirxml
  | Table
  | Trace
  | Clear
  | StartGroup
  | StartGroupCollapsed
  | EndGroup
  | Assert
  | Profile
  | ProfileEnd
  | Count
  | TimeEnd
  | Verbose

derive instance eqMessageType :: Eq MessageType
derive instance genericMessageType :: Generic MessageType _
instance showMessageType :: Show MessageType where show = genericShow

messageTypeOfString :: String -> MessageType
messageTypeOfString "debug" = Debug
messageTypeOfString "info" = Info
messageTypeOfString "error" = Error
messageTypeOfString "warning" = Warning
messageTypeOfString "dir" = Dir
messageTypeOfString "dirxml" = Dirxml
messageTypeOfString "table" = Table
messageTypeOfString "trace" = Trace
messageTypeOfString "clear" = Clear
messageTypeOfString "startGroup" = StartGroup
messageTypeOfString "startGroupCollapsed" = StartGroupCollapsed
messageTypeOfString "endGroup" = EndGroup
messageTypeOfString "assert" = Assert
messageTypeOfString "profile" = Profile
messageTypeOfString "profileEnd" = ProfileEnd
messageTypeOfString "count" = Count
messageTypeOfString "timeEnd" = TimeEnd
messageTypeOfString "verbose" = Verbose
messageTypeOfString _ = Log

messageTypeString :: MessageType -> String
messageTypeString Log = "log"
messageTypeString Debug = "debug"
messageTypeString Info = "info"
messageTypeString Error = "error"
messageTypeString Warning = "warning"
messageTypeString Dir = "dir"
messageTypeString Dirxml = "dirxml"
messageTypeString Table = "table"
messageTypeString Trace = "trace"
messageTypeString Clear = "clear"
messageTypeString StartGroup = "startGroup"
messageTypeString StartGroupCollapsed = "startGroupCollapsed"
messageTypeString EndGroup = "endGroup"
messageTypeString Assert = "assert"
messageTypeString Profile = "profile"
messageTypeString ProfileEnd = "profileEnd"
messageTypeString Count = "count"
messageTypeString TimeEnd = "timeEnd"
messageTypeString Verbose = "verbose"

foreign import data ConsoleMessage :: Type

instance consoleMessageForeign :: ReadForeign ConsoleMessage where
  readImpl = pure <<< unsafeFromForeign

foreign import args :: ConsoleMessage -> Array (Handle Foreign)
foreign import text :: ConsoleMessage -> String
foreign import _location :: ConsoleMessage -> Foreign
foreign import _stackTrace :: ConsoleMessage -> Array Foreign
foreign import _messageType :: ConsoleMessage -> String

messageType :: ConsoleMessage -> MessageType
messageType = _messageType >>> messageTypeOfString

stackTrace :: ConsoleMessage -> Array Location
stackTrace = _stackTrace >>> map locationFromJs >>> Array.catMaybes

location :: ConsoleMessage -> Maybe Location
location = _location >>> locationFromJs
