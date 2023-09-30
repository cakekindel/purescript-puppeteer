module Puppeteer.Screenshot
  ( ScreenshotFormat(..)
  , ScreenshotOptions
  , prepareScreenshotOptions
  , defaultScreenshot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Puppeteer.FFI as FFI
import Simple.JSON (writeImpl)

data Encoding
  = Base64
  | Binary

data ScreenshotFormat
  = Jpeg
  | Png
  | Webp

prepareScreenshotFormat :: ScreenshotFormat -> Foreign
prepareScreenshotFormat Jpeg = writeImpl "jpeg"
prepareScreenshotFormat Png = writeImpl "png"
prepareScreenshotFormat Webp = writeImpl "webp"

type ScreenshotOptions =
  { captureBeyondViewport :: Maybe Boolean
  , fullPage :: Maybe Boolean
  , omitBackground :: Maybe Boolean
  , optimizeForSpeed :: Maybe Boolean
  , quality :: Maybe Number
  , format :: Maybe ScreenshotFormat
  }

defaultScreenshot :: ScreenshotOptions
defaultScreenshot =
  { captureBeyondViewport: Nothing
  , fullPage: Nothing
  , omitBackground: Nothing
  , optimizeForSpeed: Nothing
  , quality: Nothing
  , format: Nothing
  }

prepareScreenshotOptions :: ScreenshotOptions -> Foreign
prepareScreenshotOptions
  { captureBeyondViewport
  , fullPage
  , omitBackground
  , optimizeForSpeed
  , quality
  , format
  } = writeImpl
  { captureBeyondViewport: FFI.maybeToUndefined captureBeyondViewport
  , fullPage: FFI.maybeToUndefined fullPage
  , omitBackground: FFI.maybeToUndefined omitBackground
  , optimizeForSpeed: FFI.maybeToUndefined optimizeForSpeed
  , quality: FFI.maybeToUndefined quality
  , format: FFI.maybeToUndefined $ map prepareScreenshotFormat format
  }
