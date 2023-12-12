module Puppeteer.Keyboard (module X, KeyMod(..), Key(..), prepareKey, keyModToString, keyModFromString, keyToString, keyFromString, up, down, doType, press) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, upFrom)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Eq.Generic (genericEq)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Ord.Generic (genericCompare)
import Data.String as String
import Data.String.CodePoints as CodePoint
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (Context(..), Keyboard)
import Puppeteer.Base (Keyboard) as X
import Puppeteer.FFI as FFI
import Simple.JSON (writeImpl)

type DownHint :: Symbol
type DownHint = "Key is being held. Invoking `Puppeteer.closeContext` will release this key"

foreign import _up :: Foreign -> Keyboard -> Effect (Promise Unit)
foreign import _down :: Foreign -> Keyboard -> Effect (Promise Unit)
foreign import _press :: Foreign -> Nullable Int -> Keyboard -> Effect (Promise Unit)
foreign import _type :: String -> Nullable Int -> Keyboard -> Effect (Promise Unit)

up :: Key -> Keyboard -> Aff Unit
up k kb = FFI.promiseToAff $ _up (prepareKey k) kb

down :: Key -> Keyboard -> Aff (Context DownHint)
down k kb = do
  FFI.promiseToAff $ _down (prepareKey k) kb
  pure $ Context (\_ -> up k kb)

press :: Key -> Keyboard -> Aff Unit
press k kb = FFI.promiseToAff $ _press (prepareKey k) Nullable.null kb

doType :: String -> Keyboard -> Aff Unit
doType s kb = FFI.promiseToAff $ _type s Nullable.null kb

data KeyMod
  = KeyModMetaLeft
  | KeyModMetaRight
  | KeyModMeta
  | KeyModShiftLeft
  | KeyModShiftRight
  | KeyModControlLeft
  | KeyModControlRight
  | KeyModAltLeft
  | KeyModAltRight
  | KeyModShift
  | KeyModControl
  | KeyModAlt

derive instance Generic KeyMod _

instance Enum KeyMod where
  pred = genericPred
  succ = genericSucc

instance Bounded KeyMod where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum KeyMod where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Ord KeyMod where
  compare = genericCompare

instance Eq KeyMod where
  eq = genericEq

data Key
  = KeyChar Char
  | KeyMod KeyMod
  | KeyPower
  | KeyEject
  | KeyAbort
  | KeyHelp
  | KeyBackspace
  | KeyTab
  | KeyEnter
  | KeyCarriageReturn
  | KeyNewline
  | KeyPause
  | KeyCapsLock
  | KeyEscape
  | KeyConvert
  | KeyNonConvert
  | KeySpace
  | KeyPageUp
  | KeyPageDown
  | KeyEnd
  | KeyHome
  | KeyArrowLeft
  | KeyArrowUp
  | KeyArrowRight
  | KeyArrowDown
  | KeySelect
  | KeyOpen
  | KeyPrintScreen
  | KeyInsert
  | KeyDelete
  | KeyContextMenu
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyNumLock
  | KeyScrollLock
  | KeyAudioVolumeMute
  | KeyAudioVolumeDown
  | KeyAudioVolumeUp
  | KeyMediaTrackNext
  | KeyMediaTrackPrevious
  | KeyMediaStop
  | KeyMediaPlayPause
  | KeyAltGraph
  | KeyProps
  | KeyCancel
  | KeyClear
  | KeyAccept
  | KeyModeChange
  | KeyAttn
  | KeyCrSel
  | KeyExSel
  | KeyEraseEof
  | KeyPlay
  | KeyZoomOut
  | KeySoftLeft
  | KeySoftRight
  | KeyCamera
  | KeyCall
  | KeyEndCall
  | KeyVolumeDown
  | KeyVolumeUp
  | KeyNumpadEnter
  | KeyNumpadMultiply
  | KeyNumpadAdd
  | KeyNumpadSubtract
  | KeyNumpadDivide
  | KeyNumpadEqual
  | KeyNumpad0
  | KeyNumpad1
  | KeyNumpad2
  | KeyNumpad3
  | KeyNumpad4
  | KeyNumpad5
  | KeyNumpad6
  | KeyNumpad7
  | KeyNumpad8
  | KeyNumpad9
  | KeyNumpadDecimal

derive instance Generic Key _

instance Enum Key where
  pred = genericPred
  succ = genericSucc

instance Bounded Key where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Key where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Ord Key where
  compare = genericCompare

instance Eq Key where
  eq = genericEq

prepareKey :: Key -> Foreign
prepareKey = writeImpl <<< keyToString

keyModFromString :: String -> Maybe KeyMod
keyModFromString s = find (eq s <<< keyModToString) (upFrom bottom :: Array KeyMod)

keyFromString :: String -> Maybe Key
keyFromString s = find (eq s <<< keyToString) (upFrom bottom :: Array Key)

keyModToString :: KeyMod -> String
keyModToString KeyModMetaLeft = "MetaLeft"
keyModToString KeyModMetaRight = "MetaRight"
keyModToString KeyModMeta = "Meta"
keyModToString KeyModShiftLeft = "ShiftLeft"
keyModToString KeyModShiftRight = "ShiftRight"
keyModToString KeyModControlLeft = "ControlLeft"
keyModToString KeyModControlRight = "ControlRight"
keyModToString KeyModAltLeft = "AltLeft"
keyModToString KeyModAltRight = "AltRight"
keyModToString KeyModShift = "Shift"
keyModToString KeyModControl = "Control"
keyModToString KeyModAlt = "Alt"

keyToString :: Key -> String
keyToString (KeyChar c) = String.fromCodePointArray [ CodePoint.codePointFromChar c ]
keyToString (KeyMod mod) = keyModToString mod
keyToString KeyPower = "Power"
keyToString KeyEject = "Eject"
keyToString KeyAbort = "Abort"
keyToString KeyHelp = "Help"
keyToString KeyBackspace = "Backspace"
keyToString KeyTab = "Tab"
keyToString KeyEnter = "Enter"
keyToString KeyCarriageReturn = "CarriageReturn"
keyToString KeyNewline = "Newline"
keyToString KeyPause = "Pause"
keyToString KeyCapsLock = "CapsLock"
keyToString KeyEscape = "Escape"
keyToString KeyConvert = "Convert"
keyToString KeyNonConvert = "NonConvert"
keyToString KeySpace = "Space"
keyToString KeyPageUp = "PageUp"
keyToString KeyPageDown = "PageDown"
keyToString KeyEnd = "End"
keyToString KeyHome = "Home"
keyToString KeyArrowLeft = "ArrowLeft"
keyToString KeyArrowUp = "ArrowUp"
keyToString KeyArrowRight = "ArrowRight"
keyToString KeyArrowDown = "ArrowDown"
keyToString KeySelect = "Select"
keyToString KeyOpen = "Open"
keyToString KeyPrintScreen = "PrintScreen"
keyToString KeyInsert = "Insert"
keyToString KeyDelete = "Delete"
keyToString KeyContextMenu = "ContextMenu"
keyToString KeyF1 = "F1"
keyToString KeyF2 = "F2"
keyToString KeyF3 = "F3"
keyToString KeyF4 = "F4"
keyToString KeyF5 = "F5"
keyToString KeyF6 = "F6"
keyToString KeyF7 = "F7"
keyToString KeyF8 = "F8"
keyToString KeyF9 = "F9"
keyToString KeyF10 = "F10"
keyToString KeyF11 = "F11"
keyToString KeyF12 = "F12"
keyToString KeyF13 = "F13"
keyToString KeyF14 = "F14"
keyToString KeyF15 = "F15"
keyToString KeyF16 = "F16"
keyToString KeyF17 = "F17"
keyToString KeyF18 = "F18"
keyToString KeyF19 = "F19"
keyToString KeyF20 = "F20"
keyToString KeyF21 = "F21"
keyToString KeyF22 = "F22"
keyToString KeyF23 = "F23"
keyToString KeyF24 = "F24"
keyToString KeyNumLock = "NumLock"
keyToString KeyScrollLock = "ScrollLock"
keyToString KeyAudioVolumeMute = "AudioVolumeMute"
keyToString KeyAudioVolumeDown = "AudioVolumeDown"
keyToString KeyAudioVolumeUp = "AudioVolumeUp"
keyToString KeyMediaTrackNext = "MediaTrackNext"
keyToString KeyMediaTrackPrevious = "MediaTrackPrevious"
keyToString KeyMediaStop = "MediaStop"
keyToString KeyMediaPlayPause = "MediaPlayPause"
keyToString KeyAltGraph = "AltGraph"
keyToString KeyProps = "Props"
keyToString KeyCancel = "Cancel"
keyToString KeyClear = "Clear"
keyToString KeyAccept = "Accept"
keyToString KeyModeChange = "ModeChange"
keyToString KeyAttn = "Attn"
keyToString KeyCrSel = "CrSel"
keyToString KeyExSel = "ExSel"
keyToString KeyEraseEof = "EraseEof"
keyToString KeyPlay = "Play"
keyToString KeyZoomOut = "ZoomOut"
keyToString KeySoftLeft = "SoftLeft"
keyToString KeySoftRight = "SoftRight"
keyToString KeyCamera = "Camera"
keyToString KeyCall = "Call"
keyToString KeyEndCall = "EndCall"
keyToString KeyVolumeDown = "VolumeDown"
keyToString KeyVolumeUp = "VolumeUp"
keyToString KeyNumpadEnter = "NumpadEnter"
keyToString KeyNumpadMultiply = "NumpadMultiply"
keyToString KeyNumpadAdd = "NumpadAdd"
keyToString KeyNumpadSubtract = "NumpadSubtract"
keyToString KeyNumpadDivide = "NumpadDivide"
keyToString KeyNumpadEqual = "NumpadEqual"
keyToString KeyNumpad0 = "Numpad0"
keyToString KeyNumpad1 = "Numpad1"
keyToString KeyNumpad2 = "Numpad2"
keyToString KeyNumpad3 = "Numpad3"
keyToString KeyNumpad4 = "Numpad4"
keyToString KeyNumpad5 = "Numpad5"
keyToString KeyNumpad6 = "Numpad6"
keyToString KeyNumpad7 = "Numpad7"
keyToString KeyNumpad8 = "Numpad8"
keyToString KeyNumpad9 = "Numpad9"
keyToString KeyNumpadDecimal = "NumpadDecimal"
