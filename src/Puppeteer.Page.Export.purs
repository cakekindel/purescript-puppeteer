module Puppeteer.Page.Export (module X, screenshot, pdf, PdfOptions, PaperFormat(..)) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Node.Buffer (Buffer)
import Puppeteer.Base (Page)
import Puppeteer.FFI as FFI
import Puppeteer.Screenshot (ScreenshotFormat(..), ScreenshotOptions) as X
import Puppeteer.Screenshot (ScreenshotOptions, prepareScreenshotOptions)
import Simple.JSON (writeImpl)

data PaperFormat
  = Letter
  | Legal
  | Tabloid
  | Ledger
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6

preparePaperFormat :: PaperFormat -> Foreign
preparePaperFormat Letter = writeImpl "letter"
preparePaperFormat Legal = writeImpl "legal"
preparePaperFormat Tabloid = writeImpl "tabloid"
preparePaperFormat Ledger = writeImpl "ledger"
preparePaperFormat A0 = writeImpl "a0"
preparePaperFormat A1 = writeImpl "a1"
preparePaperFormat A2 = writeImpl "a2"
preparePaperFormat A3 = writeImpl "a3"
preparePaperFormat A4 = writeImpl "a4"
preparePaperFormat A5 = writeImpl "a5"
preparePaperFormat A6 = writeImpl "a6"

type PdfOptions =
  { displayHeaderFooter :: Maybe Boolean
  , footerTemplate :: Maybe String
  , headerTemplate :: Maybe String
  , paperFormat :: Maybe PaperFormat
  , height :: Maybe String
  , width :: Maybe String
  , landscape :: Maybe Boolean
  , omitBackground :: Maybe Boolean
  , printBackground :: Maybe Boolean
  , scale :: Maybe Number
  , margin ::
      Maybe
        { top :: Maybe String
        , left :: Maybe String
        , bottom :: Maybe String
        , right :: Maybe String
        }
  }

preparePdfOptions :: PdfOptions -> Foreign
preparePdfOptions
  { displayHeaderFooter
  , footerTemplate
  , headerTemplate
  , paperFormat
  , height
  , width
  , landscape
  , omitBackground
  , printBackground
  , scale
  , margin
  } =
  let
    margin' { top, left, bottom, right } =
      writeImpl
        { top: FFI.maybeToUndefined top
        , bottom: FFI.maybeToUndefined bottom
        , left: FFI.maybeToUndefined left
        , right: FFI.maybeToUndefined right
        }
  in
    writeImpl
      { displayHeaderFooter: FFI.maybeToUndefined displayHeaderFooter
      , footerTemplate: FFI.maybeToUndefined footerTemplate
      , headerTemplate: FFI.maybeToUndefined headerTemplate
      , paperFormat: FFI.maybeToUndefined $ map preparePaperFormat paperFormat
      , height: FFI.maybeToUndefined height
      , width: FFI.maybeToUndefined width
      , landscape: FFI.maybeToUndefined landscape
      , omitBackground: FFI.maybeToUndefined omitBackground
      , printBackground: FFI.maybeToUndefined printBackground
      , scale: FFI.maybeToUndefined scale
      , margin: FFI.maybeToUndefined $ map margin' margin
      }

foreign import _screenshot :: Foreign -> Page -> Promise Buffer
foreign import _pdf :: Foreign -> Page -> Promise Buffer

screenshot :: ScreenshotOptions -> Page -> Aff Buffer
screenshot o = Promise.toAff <<< _screenshot (prepareScreenshotOptions o)

pdf :: PdfOptions -> Page -> Aff Buffer
pdf o = Promise.toAff <<< _pdf (preparePdfOptions o)
