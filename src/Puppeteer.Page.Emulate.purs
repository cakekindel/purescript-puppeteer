module Puppeteer.Page.Emulate
  ( ThrottleFactor(..)
  , noThrottling
  , cpuThrottling
  , device
  , Device(..)
  , idle
  , Idle(..)
  , timezone
  , print
  , network
  , VisionDeficiency(..)
  , visionDeficiency
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (Context(..), Handle, Page)
import Simple.JSON (undefined, writeImpl)

type NetworkConditions =
  { download :: Number
  , upload :: Number
  , latency :: Number
  }

type Geolocation =
  { accuracy :: Maybe Number
  , latitude :: Number
  , longitude :: Number
  }

data Idle = UserInactive | ScreenLocked | NotIdle

newtype ThrottleFactor = ThrottleFactor Number

derive instance ntthrottlefactor :: Newtype ThrottleFactor _

noThrottling :: ThrottleFactor
noThrottling = ThrottleFactor 1.0

foreign import _idle :: Foreign -> Page -> Promise Unit
foreign import _knownDevice :: String -> Foreign
foreign import _emulate :: Foreign -> Page -> Promise Unit
foreign import _cpuThrottling :: Number -> Page -> Promise Unit

foreign import _emulatePrint :: Page -> Promise Unit
foreign import _unemulatePrint :: Page -> Promise Unit

foreign import _emulateNetwork :: NetworkConditions -> Page -> Promise Unit
foreign import _unemulateNetwork :: Page -> Promise Unit

foreign import _emulateTimezone :: String -> Page -> Promise Unit
foreign import _unemulateTimezone :: Page -> Promise Unit

foreign import _emulateVisionDeficiency :: String -> Page -> Promise Unit
foreign import _unemulateVisionDeficiency :: Page -> Promise Unit

foreign import _setGeolocation :: Geolocation -> Page -> Promise Unit
foreign import _unsetGeolocation :: Page -> Promise Unit

foreign import _setJavascriptDisabled :: Page -> Promise Unit
foreign import _unsetJavascriptDisabled :: Page -> Promise Unit

foreign import _setOffline :: Page -> Promise Unit
foreign import _unsetOffline :: Page -> Promise Unit

foreign import _setUserAgent :: String -> Page -> Promise Unit

type PrintHint :: Symbol
type PrintHint = "Print mode is being emulated. Invoking `Puppeteer.closeContext` will undo this emulation."

type NetworkHint :: Symbol
type NetworkHint = "Alternate network conditions are being emulated. Invoking `Puppeteer.closeContext` will undo this emulation."

type TimezoneHint :: Symbol
type TimezoneHint = "Alternate timezone is being emulated. Invoking `Puppeteer.closeContext` will undo this emulation."

type VisionDeficiencyHint :: Symbol
type VisionDeficiencyHint = "Vision deficiency is being emulated. Invoking `Puppeteer.closeContext` will undo this emulation."

type CpuThrottleHint :: Symbol
type CpuThrottleHint = "CPU throttling is being emulated. Invoking `Puppeteer.closeContext` will undo this emulation."

type IdleHint :: Symbol
type IdleHint = "User idling is being emulated. Invoking `Puppeteer.closeContext` will undo this emulation."

device :: Device -> Page -> Aff Unit
device d p = Promise.toAff $ _emulate (_knownDevice <<< knownDeviceString $ d) p

cpuThrottling :: ThrottleFactor -> Page -> Aff (Context CpuThrottleHint)
cpuThrottling t p = do
  Promise.toAff $ _cpuThrottling (unwrap t) p
  pure $ Context (\_ -> Promise.toAff $ _cpuThrottling 1.0 p)

idle :: Idle -> Page -> Aff (Context IdleHint)
idle UserInactive p = map (const $ Context (\_ -> unidle p)) <<< Promise.toAff <<< _idle (writeImpl { isUserActive: false, isScreenUnlocked: true }) $ p
idle ScreenLocked p = map (const $ Context (\_ -> unidle p)) <<< Promise.toAff <<< _idle (writeImpl { isScreenUnlocked: false, isUserActive: false }) $ p
idle NotIdle p = map (const mempty) $ unidle p

unidle :: Page -> Aff Unit
unidle = void <<< Promise.toAff <<< _idle undefined

print :: Page -> Aff (Context PrintHint)
print p = do
  Promise.toAff $ _emulatePrint p
  pure $ Context (\_ -> Promise.toAff $ _unemulatePrint p)

network :: NetworkConditions -> Page -> Aff (Context NetworkHint)
network n p = do
  Promise.toAff $ _emulateNetwork n p
  pure $ Context (\_ -> Promise.toAff $ _unemulateNetwork p)

--| https://pptr.dev/api/puppeteer.page.emulatetimezone
timezone :: String -> Page -> Aff (Context TimezoneHint)
timezone tz p = do
  Promise.toAff $ _emulateTimezone tz p
  pure $ Context (\_ -> Promise.toAff $ _unemulateTimezone p)

visionDeficiency :: VisionDeficiency -> Page -> Aff (Context VisionDeficiencyHint)
visionDeficiency d p = do
  Promise.toAff $ _emulateVisionDeficiency (visionDeficiencyString d) p
  pure $ Context (\_ -> Promise.toAff $ _unemulateVisionDeficiency p)

data VisionDeficiency
  = BlurredVision
  | ReducedContrast
  | Achromatopsia
  | Deuteranopia
  | Protanopia
  | Tritanopia

visionDeficiencyString :: VisionDeficiency -> String
visionDeficiencyString BlurredVision = "blurredVision"
visionDeficiencyString ReducedContrast = "reducedContrast"
visionDeficiencyString Achromatopsia = "achromatopsia"
visionDeficiencyString Deuteranopia = "deuteranopia"
visionDeficiencyString Protanopia = "protanopia"
visionDeficiencyString Tritanopia = "tritanopia"

data Device
  = BlackberryPlayBook
  | BlackberryPlayBookLandscape
  | BlackBerryZ30
  | BlackBerryZ30Landscape
  | GalaxyNote3
  | GalaxyNote3Landscape
  | GalaxyNoteII
  | GalaxyNoteIILandscape
  | GalaxySIII
  | GalaxySIIILandscape
  | GalaxyS5
  | GalaxyS5Landscape
  | GalaxyS8
  | GalaxyS8Landscape
  | GalaxyS9Plus
  | GalaxyS9PlusLandscape
  | GalaxyTabS4
  | GalaxyTabS4Landscape
  | IPad
  | IPadLandscape
  | IPadGen6
  | IPadGen6Landscape
  | IPadGen7
  | IPadGen7Landscape
  | IPadMini
  | IPadMiniLandscape
  | IPadPro
  | IPadProLandscape
  | IPadPro11
  | IPadPro11Landscape
  | IPhone4
  | IPhone4Landscape
  | IPhone5
  | IPhone5Landscape
  | IPhone6
  | IPhone6Landscape
  | IPhone6Plus
  | IPhone6PlusLandscape
  | IPhone7
  | IPhone7Landscape
  | IPhone7Plus
  | IPhone7PlusLandscape
  | IPhone8
  | IPhone8Landscape
  | IPhone8Plus
  | IPhone8PlusLandscape
  | IPhoneSE
  | IPhoneSELandscape
  | IPhoneX
  | IPhoneXLandscape
  | IPhoneXR
  | IPhoneXRLandscape
  | IPhone11
  | IPhone11Landscape
  | IPhone11Pro
  | IPhone11ProLandscape
  | IPhone11ProMax
  | IPhone11ProMaxLandscape
  | IPhone12
  | IPhone12Landscape
  | IPhone12Pro
  | IPhone12ProLandscape
  | IPhone12ProMax
  | IPhone12ProMaxLandscape
  | IPhone12Mini
  | IPhone12MiniLandscape
  | IPhone13
  | IPhone13Landscape
  | IPhone13Pro
  | IPhone13ProLandscape
  | IPhone13ProMax
  | IPhone13ProMaxLandscape
  | IPhone13Mini
  | IPhone13MiniLandscape
  | JioPhone2
  | JioPhone2Landscape
  | KindleFireHDX
  | KindleFireHDXLandscape
  | LGOptimusL70
  | LGOptimusL70Landscape
  | MicrosoftLumia550
  | MicrosoftLumia950
  | MicrosoftLumia950Landscape
  | Nexus10
  | Nexus10Landscape
  | Nexus4
  | Nexus4Landscape
  | Nexus5
  | Nexus5Landscape
  | Nexus5X
  | Nexus5XLandscape
  | Nexus6
  | Nexus6Landscape
  | Nexus6P
  | Nexus6PLandscape
  | Nexus7
  | Nexus7Landscape
  | NokiaLumia520
  | NokiaLumia520Landscape
  | NokiaN9
  | NokiaN9Landscape
  | Pixel2
  | Pixel2Landscape
  | Pixel2XL
  | Pixel2XLLandscape
  | Pixel3
  | Pixel3Landscape
  | Pixel4
  | Pixel4Landscape
  | Pixel4a5G
  | Pixel4a5GLandscape
  | Pixel5
  | Pixel5Landscape
  | MotoG4
  | MotoG4Landscape

knownDeviceString :: Device -> String
knownDeviceString BlackberryPlayBook = "Blackberry PlayBook"
knownDeviceString BlackberryPlayBookLandscape = "Blackberry PlayBook landscape"
knownDeviceString BlackBerryZ30 = "BlackBerry Z30"
knownDeviceString BlackBerryZ30Landscape = "BlackBerry Z30 landscape"
knownDeviceString GalaxyNote3 = "Galaxy Note 3"
knownDeviceString GalaxyNote3Landscape = "Galaxy Note 3 landscape"
knownDeviceString GalaxyNoteII = "Galaxy Note II"
knownDeviceString GalaxyNoteIILandscape = "Galaxy Note II landscape"
knownDeviceString GalaxySIII = "Galaxy S III"
knownDeviceString GalaxySIIILandscape = "Galaxy S III landscape"
knownDeviceString GalaxyS5 = "Galaxy S5"
knownDeviceString GalaxyS5Landscape = "Galaxy S5 landscape"
knownDeviceString GalaxyS8 = "Galaxy S8"
knownDeviceString GalaxyS8Landscape = "Galaxy S8 landscape"
knownDeviceString GalaxyS9Plus = "Galaxy S9+"
knownDeviceString GalaxyS9PlusLandscape = "Galaxy S9+ landscape"
knownDeviceString GalaxyTabS4 = "Galaxy Tab S4"
knownDeviceString GalaxyTabS4Landscape = "Galaxy Tab S4 landscape"
knownDeviceString IPad = "iPad"
knownDeviceString IPadLandscape = "iPad landscape"
knownDeviceString IPadGen6 = "iPad (gen 6)"
knownDeviceString IPadGen6Landscape = "iPad (gen 6) landscape"
knownDeviceString IPadGen7 = "iPad (gen 7)"
knownDeviceString IPadGen7Landscape = "iPad (gen 7) landscape"
knownDeviceString IPadMini = "iPad Mini"
knownDeviceString IPadMiniLandscape = "iPad Mini landscape"
knownDeviceString IPadPro = "iPad Pro"
knownDeviceString IPadProLandscape = "iPad Pro landscape"
knownDeviceString IPadPro11 = "iPad Pro 11"
knownDeviceString IPadPro11Landscape = "iPad Pro 11 landscape"
knownDeviceString IPhone4 = "iPhone 4"
knownDeviceString IPhone4Landscape = "iPhone 4 landscape"
knownDeviceString IPhone5 = "iPhone 5"
knownDeviceString IPhone5Landscape = "iPhone 5 landscape"
knownDeviceString IPhone6 = "iPhone 6"
knownDeviceString IPhone6Landscape = "iPhone 6 landscape"
knownDeviceString IPhone6Plus = "iPhone 6 Plus"
knownDeviceString IPhone6PlusLandscape = "iPhone 6 Plus landscape"
knownDeviceString IPhone7 = "iPhone 7"
knownDeviceString IPhone7Landscape = "iPhone 7 landscape"
knownDeviceString IPhone7Plus = "iPhone 7 Plus"
knownDeviceString IPhone7PlusLandscape = "iPhone 7 Plus landscape"
knownDeviceString IPhone8 = "iPhone 8"
knownDeviceString IPhone8Landscape = "iPhone 8 landscape"
knownDeviceString IPhone8Plus = "iPhone 8 Plus"
knownDeviceString IPhone8PlusLandscape = "iPhone 8 Plus landscape"
knownDeviceString IPhoneSE = "iPhone SE"
knownDeviceString IPhoneSELandscape = "iPhone SE landscape"
knownDeviceString IPhoneX = "iPhone X"
knownDeviceString IPhoneXLandscape = "iPhone X landscape"
knownDeviceString IPhoneXR = "iPhone XR"
knownDeviceString IPhoneXRLandscape = "iPhone XR landscape"
knownDeviceString IPhone11 = "iPhone 11"
knownDeviceString IPhone11Landscape = "iPhone 11 landscape"
knownDeviceString IPhone11Pro = "iPhone 11 Pro"
knownDeviceString IPhone11ProLandscape = "iPhone 11 Pro landscape"
knownDeviceString IPhone11ProMax = "iPhone 11 Pro Max"
knownDeviceString IPhone11ProMaxLandscape = "iPhone 11 Pro Max landscape"
knownDeviceString IPhone12 = "iPhone 12"
knownDeviceString IPhone12Landscape = "iPhone 12 landscape"
knownDeviceString IPhone12Pro = "iPhone 12 Pro"
knownDeviceString IPhone12ProLandscape = "iPhone 12 Pro landscape"
knownDeviceString IPhone12ProMax = "iPhone 12 Pro Max"
knownDeviceString IPhone12ProMaxLandscape = "iPhone 12 Pro Max landscape"
knownDeviceString IPhone12Mini = "iPhone 12 Mini"
knownDeviceString IPhone12MiniLandscape = "iPhone 12 Mini landscape"
knownDeviceString IPhone13 = "iPhone 13"
knownDeviceString IPhone13Landscape = "iPhone 13 landscape"
knownDeviceString IPhone13Pro = "iPhone 13 Pro"
knownDeviceString IPhone13ProLandscape = "iPhone 13 Pro landscape"
knownDeviceString IPhone13ProMax = "iPhone 13 Pro Max"
knownDeviceString IPhone13ProMaxLandscape = "iPhone 13 Pro Max landscape"
knownDeviceString IPhone13Mini = "iPhone 13 Mini"
knownDeviceString IPhone13MiniLandscape = "iPhone 13 Mini landscape"
knownDeviceString JioPhone2 = "JioPhone 2"
knownDeviceString JioPhone2Landscape = "JioPhone 2 landscape"
knownDeviceString KindleFireHDX = "Kindle Fire HDX"
knownDeviceString KindleFireHDXLandscape = "Kindle Fire HDX landscape"
knownDeviceString LGOptimusL70 = "LG Optimus L70"
knownDeviceString LGOptimusL70Landscape = "LG Optimus L70 landscape"
knownDeviceString MicrosoftLumia550 = "Microsoft Lumia 550"
knownDeviceString MicrosoftLumia950 = "Microsoft Lumia 950"
knownDeviceString MicrosoftLumia950Landscape = "Microsoft Lumia 950 landscape"
knownDeviceString Nexus10 = "Nexus 10"
knownDeviceString Nexus10Landscape = "Nexus 10 landscape"
knownDeviceString Nexus4 = "Nexus 4"
knownDeviceString Nexus4Landscape = "Nexus 4 landscape"
knownDeviceString Nexus5 = "Nexus 5"
knownDeviceString Nexus5Landscape = "Nexus 5 landscape"
knownDeviceString Nexus5X = "Nexus 5X"
knownDeviceString Nexus5XLandscape = "Nexus 5X landscape"
knownDeviceString Nexus6 = "Nexus 6"
knownDeviceString Nexus6Landscape = "Nexus 6 landscape"
knownDeviceString Nexus6P = "Nexus 6P"
knownDeviceString Nexus6PLandscape = "Nexus 6P landscape"
knownDeviceString Nexus7 = "Nexus 7"
knownDeviceString Nexus7Landscape = "Nexus 7 landscape"
knownDeviceString NokiaLumia520 = "Nokia Lumia 520"
knownDeviceString NokiaLumia520Landscape = "Nokia Lumia 520 landscape"
knownDeviceString NokiaN9 = "Nokia N9"
knownDeviceString NokiaN9Landscape = "Nokia N9 landscape"
knownDeviceString Pixel2 = "Pixel 2"
knownDeviceString Pixel2Landscape = "Pixel 2 landscape"
knownDeviceString Pixel2XL = "Pixel 2 XL"
knownDeviceString Pixel2XLLandscape = "Pixel 2 XL landscape"
knownDeviceString Pixel3 = "Pixel 3"
knownDeviceString Pixel3Landscape = "Pixel 3 landscape"
knownDeviceString Pixel4 = "Pixel 4"
knownDeviceString Pixel4Landscape = "Pixel 4 landscape"
knownDeviceString Pixel4a5G = "Pixel 4a (5G)"
knownDeviceString Pixel4a5GLandscape = "Pixel 4a (5G) landscape"
knownDeviceString Pixel5 = "Pixel 5"
knownDeviceString Pixel5Landscape = "Pixel 5 landscape"
knownDeviceString MotoG4 = "Moto G4"
knownDeviceString MotoG4Landscape = "Moto G4 landscape'"
