module Puppeteer.Keyboard (module X, KeyMod(..), Key(..), prepareKey, up, down, doType, press) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
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
import Simple.JSON (writeImpl)

type DownHint :: Symbol
type DownHint = "Key is being held. Invoking `Puppeteer.closeContext` will release this key"

foreign import _up :: Foreign -> Keyboard -> Effect (Promise Unit)
foreign import _down :: Foreign -> Keyboard -> Effect (Promise Unit)
foreign import _press :: Foreign -> Nullable Int -> Keyboard -> Effect (Promise Unit)
foreign import _type :: String -> Nullable Int -> Keyboard -> Effect (Promise Unit)

up :: Key -> Keyboard -> Aff Unit
up k kb = Promise.toAffE $ _up (prepareKey k) kb

down :: Key -> Keyboard -> Aff (Context DownHint)
down k kb = do
  Promise.toAffE $ _down (prepareKey k) kb
  pure $ Context (\_ -> up k kb)

press :: Key -> Keyboard -> Aff Unit
press k kb = Promise.toAffE $ _press (prepareKey k) Nullable.null kb

doType :: String -> Keyboard -> Aff Unit
doType s kb = Promise.toAffE $ _type s Nullable.null kb

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

derive instance genericKeyMod :: Generic KeyMod _
instance ordKeyMod :: Ord KeyMod where
  compare = genericCompare

instance eqKeyMod :: Eq KeyMod where
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

derive instance genericKey :: Generic Key _
instance ordKey :: Ord Key where
  compare = genericCompare

instance eqKey :: Eq Key where
  eq = genericEq

prepareKey :: Key -> Foreign
prepareKey = writeImpl <<< prepareKey_

prepareKeyMod_ :: KeyMod -> String
prepareKeyMod_ KeyModMetaLeft = "MetaLeft"
prepareKeyMod_ KeyModMetaRight = "MetaRight"
prepareKeyMod_ KeyModMeta = "Meta"
prepareKeyMod_ KeyModShiftLeft = "ShiftLeft"
prepareKeyMod_ KeyModShiftRight = "ShiftRight"
prepareKeyMod_ KeyModControlLeft = "ControlLeft"
prepareKeyMod_ KeyModControlRight = "ControlRight"
prepareKeyMod_ KeyModAltLeft = "AltLeft"
prepareKeyMod_ KeyModAltRight = "AltRight"
prepareKeyMod_ KeyModShift = "Shift"
prepareKeyMod_ KeyModControl = "Control"
prepareKeyMod_ KeyModAlt = "Alt"

prepareKey_ :: Key -> String
prepareKey_ (KeyChar c) = String.fromCodePointArray [ CodePoint.codePointFromChar c ]
prepareKey_ (KeyMod mod) = prepareKeyMod_ mod
prepareKey_ KeyPower = "Power"
prepareKey_ KeyEject = "Eject"
prepareKey_ KeyAbort = "Abort"
prepareKey_ KeyHelp = "Help"
prepareKey_ KeyBackspace = "Backspace"
prepareKey_ KeyTab = "Tab"
prepareKey_ KeyEnter = "Enter"
prepareKey_ KeyCarriageReturn = "CarriageReturn"
prepareKey_ KeyNewline = "Newline"
prepareKey_ KeyPause = "Pause"
prepareKey_ KeyCapsLock = "CapsLock"
prepareKey_ KeyEscape = "Escape"
prepareKey_ KeyConvert = "Convert"
prepareKey_ KeyNonConvert = "NonConvert"
prepareKey_ KeySpace = "Space"
prepareKey_ KeyPageUp = "PageUp"
prepareKey_ KeyPageDown = "PageDown"
prepareKey_ KeyEnd = "End"
prepareKey_ KeyHome = "Home"
prepareKey_ KeyArrowLeft = "ArrowLeft"
prepareKey_ KeyArrowUp = "ArrowUp"
prepareKey_ KeyArrowRight = "ArrowRight"
prepareKey_ KeyArrowDown = "ArrowDown"
prepareKey_ KeySelect = "Select"
prepareKey_ KeyOpen = "Open"
prepareKey_ KeyPrintScreen = "PrintScreen"
prepareKey_ KeyInsert = "Insert"
prepareKey_ KeyDelete = "Delete"
prepareKey_ KeyContextMenu = "ContextMenu"
prepareKey_ KeyF1 = "F1"
prepareKey_ KeyF2 = "F2"
prepareKey_ KeyF3 = "F3"
prepareKey_ KeyF4 = "F4"
prepareKey_ KeyF5 = "F5"
prepareKey_ KeyF6 = "F6"
prepareKey_ KeyF7 = "F7"
prepareKey_ KeyF8 = "F8"
prepareKey_ KeyF9 = "F9"
prepareKey_ KeyF10 = "F10"
prepareKey_ KeyF11 = "F11"
prepareKey_ KeyF12 = "F12"
prepareKey_ KeyF13 = "F13"
prepareKey_ KeyF14 = "F14"
prepareKey_ KeyF15 = "F15"
prepareKey_ KeyF16 = "F16"
prepareKey_ KeyF17 = "F17"
prepareKey_ KeyF18 = "F18"
prepareKey_ KeyF19 = "F19"
prepareKey_ KeyF20 = "F20"
prepareKey_ KeyF21 = "F21"
prepareKey_ KeyF22 = "F22"
prepareKey_ KeyF23 = "F23"
prepareKey_ KeyF24 = "F24"
prepareKey_ KeyNumLock = "NumLock"
prepareKey_ KeyScrollLock = "ScrollLock"
prepareKey_ KeyAudioVolumeMute = "AudioVolumeMute"
prepareKey_ KeyAudioVolumeDown = "AudioVolumeDown"
prepareKey_ KeyAudioVolumeUp = "AudioVolumeUp"
prepareKey_ KeyMediaTrackNext = "MediaTrackNext"
prepareKey_ KeyMediaTrackPrevious = "MediaTrackPrevious"
prepareKey_ KeyMediaStop = "MediaStop"
prepareKey_ KeyMediaPlayPause = "MediaPlayPause"
prepareKey_ KeyAltGraph = "AltGraph"
prepareKey_ KeyProps = "Props"
prepareKey_ KeyCancel = "Cancel"
prepareKey_ KeyClear = "Clear"
prepareKey_ KeyAccept = "Accept"
prepareKey_ KeyModeChange = "ModeChange"
prepareKey_ KeyAttn = "Attn"
prepareKey_ KeyCrSel = "CrSel"
prepareKey_ KeyExSel = "ExSel"
prepareKey_ KeyEraseEof = "EraseEof"
prepareKey_ KeyPlay = "Play"
prepareKey_ KeyZoomOut = "ZoomOut"
prepareKey_ KeySoftLeft = "SoftLeft"
prepareKey_ KeySoftRight = "SoftRight"
prepareKey_ KeyCamera = "Camera"
prepareKey_ KeyCall = "Call"
prepareKey_ KeyEndCall = "EndCall"
prepareKey_ KeyVolumeDown = "VolumeDown"
prepareKey_ KeyVolumeUp = "VolumeUp"
prepareKey_ KeyNumpadEnter = "NumpadEnter"
prepareKey_ KeyNumpadMultiply = "NumpadMultiply"
prepareKey_ KeyNumpadAdd = "NumpadAdd"
prepareKey_ KeyNumpadSubtract = "NumpadSubtract"
prepareKey_ KeyNumpadDivide = "NumpadDivide"
prepareKey_ KeyNumpadEqual = "NumpadEqual"
prepareKey_ KeyNumpad0 = "Numpad0"
prepareKey_ KeyNumpad1 = "Numpad1"
prepareKey_ KeyNumpad2 = "Numpad2"
prepareKey_ KeyNumpad3 = "Numpad3"
prepareKey_ KeyNumpad4 = "Numpad4"
prepareKey_ KeyNumpad5 = "Numpad5"
prepareKey_ KeyNumpad6 = "Numpad6"
prepareKey_ KeyNumpad7 = "Numpad7"
prepareKey_ KeyNumpad8 = "Numpad8"
prepareKey_ KeyNumpad9 = "Numpad9"
prepareKey_ KeyNumpadDecimal = "NumpadDecimal"
