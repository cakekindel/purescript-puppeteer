module Puppeteer.Handle.HTML where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (class IsElement, Handle)
import Puppeteer.Eval as Eval
import Puppeteer.FFI as FFI
import Puppeteer.Handle (getProperties, toHTML, unsafeCoerceHandle)
import Web.CSSOM.CSSStyleDeclaration (CSSStyleDeclaration)
import Web.DOM.HTMLCollection (HTMLCollection)
import Web.HTML as HTML

equals :: forall a. IsElement a => Handle a -> Handle a -> Aff Boolean
equals a b = Eval.unsafeRunJs1 "(a, b) => a === b" a b

contains :: forall a. IsElement a => Handle a -> Handle a -> Aff Boolean
contains a b = Eval.unsafeRunJs1 "(a, b) => a.contains(b)" a b

tagName :: forall a. IsElement a => Handle a -> Aff String
tagName = Eval.unsafeRunJs0 "e => e.tagName"

textContent :: forall a. IsElement a => Handle a -> Aff String
textContent = Eval.unsafeRunJs0 "e => e.textContent"

innerHTML :: forall a. IsElement a => Handle a -> Aff String
innerHTML = Eval.unsafeRunJs0 "e => e.innerHTML"

outerHTML :: forall a. IsElement a => Handle a -> Aff String
outerHTML = Eval.unsafeRunJs0 "e => e.outerHTML"

id :: forall a. IsElement a => Handle a -> Aff String
id = Eval.unsafeRunJs0 "e => e.id"

classList :: forall a. IsElement a => Handle a -> Aff (Array String)
classList = Eval.unsafeRunJs0 "e => Array.from(e.classList)"

scrollTop :: forall a. IsElement a => Handle a -> Aff (Array Number)
scrollTop = Eval.unsafeRunJs0 "e => e.scrollTop"

scrollLeft :: forall a. IsElement a => Handle a -> Aff (Array Number)
scrollLeft = Eval.unsafeRunJs0 "e => e.scrollLeft"

scrollWidth :: forall a. IsElement a => Handle a -> Aff (Array Number)
scrollWidth = Eval.unsafeRunJs0 "e => e.scrollWidth"

scrollHeight :: forall a. IsElement a => Handle a -> Aff (Array Number)
scrollHeight = Eval.unsafeRunJs0 "e => e.scrollHeight"

clientTop :: forall a. IsElement a => Handle a -> Aff (Array Number)
clientTop = Eval.unsafeRunJs0 "e => e.clientTop"

clientLeft :: forall a. IsElement a => Handle a -> Aff (Array Number)
clientLeft = Eval.unsafeRunJs0 "e => e.clientLeft"

clientWidth :: forall a. IsElement a => Handle a -> Aff (Array Number)
clientWidth = Eval.unsafeRunJs0 "e => e.clientWidth"

clientHeight :: forall a. IsElement a => Handle a -> Aff (Array Number)
clientHeight = Eval.unsafeRunJs0 "e => e.clientHeight"

offsetTop :: forall a. IsElement a => Handle a -> Aff (Array Number)
offsetTop = Eval.unsafeRunJs0 "e => e.offsetTop"

offsetLeft :: forall a. IsElement a => Handle a -> Aff (Array Number)
offsetLeft = Eval.unsafeRunJs0 "e => e.offsetLeft"

offsetWidth :: forall a. IsElement a => Handle a -> Aff (Array Number)
offsetWidth = Eval.unsafeRunJs0 "e => e.offsetWidth"

offsetHeight :: forall a. IsElement a => Handle a -> Aff (Array Number)
offsetHeight = Eval.unsafeRunJs0 "e => e.offsetHeight"

attrs :: forall a. IsElement a => Handle a -> Aff (Map String String)
attrs = let
  js = String.joinWith "\n"
        [ "e => Array.from(e.attributes)"
        , "          .reduce("
        , "            (m, a) => [...m, {k: a.name, v: a.value}],"
        , "            [],"
        , "          )"
        , "          .filter(({k}) => k)"
        ]
  in
    map FFI.makeMap <<< Eval.unsafeRunJs0 @(Array {k :: String, v :: String}) js

computedStyle :: forall a. IsElement a => Handle a -> Aff (Map String String)
computedStyle = let
    js = String.joinWith "\n"
           [ "e => {"
           , "  const s = window.getComputedStyle(e)"
           , "  const a = []"
           , "  for (let i = 0; i < s.length; i++) {"
           , "    const k = s.item(i)"
           , "    const v = s.getPropertyValue(k)"
           , "    a.push({k, v})"
           , "  }"
           , ""
           , "  return a"
           , "}"
           ]
  in
    map FFI.makeMap <<< Eval.unsafeRunJs0 @(Array {k :: String, v :: String}) js

value :: Handle HTML.HTMLInputElement -> Aff String
value = Eval.unsafeRunJs0 "e => e.value"

textAreaValue :: Handle HTML.HTMLTextAreaElement -> Aff String
textAreaValue = Eval.unsafeRunJs0 "e => e.value"

children :: forall a. IsElement a => Handle a -> Aff (Array (Handle HTML.HTMLElement))
children h = do
  coll <- Eval.unsafeRunJsHandle0 @HTMLCollection "e => e.children" h
  length <- Eval.unsafeRunJs0 @Int "c => c.length" coll
  props <- getProperties coll
  els <- sequence
    $ Array.catMaybes
    $ map toHTML
        <$> flip Map.lookup props
        <$> Int.toStringAs Int.decimal
        <$> Array.range 0 (length - 1)
  pure $ Array.catMaybes els

style :: forall a. IsElement a => Handle a -> Aff (Map String String)
style h =
  let
    prop :: Handle CSSStyleDeclaration -> Int -> Aff (Tuple String String)
    prop css ix = do
      prop' <- Eval.unsafeRunJs0 @String ("c => c.item('" <> Int.toStringAs Int.decimal ix <> "')") css
      val <- Eval.unsafeRunJs0 @String ("c => c.getPropertyValue('" <> prop' <> "')") css
      pure $ Tuple prop' val
  in
    do
      css <- Eval.unsafeRunJsHandle0 @CSSStyleDeclaration "e => e.style" h
      length <- Eval.unsafeRunJs0 @Int "c => c.length" css
      props <- sequence $ prop css <$> Array.range 0 (length - 1)
      pure $ Map.fromFoldable props

parent :: forall a. IsElement a => Handle a -> Aff (Maybe (Handle HTML.HTMLElement))
parent h = do
  parentForeign <- Eval.unsafeRunJsHandle0 @Foreign "e => e.parentElement" h
  toHTML parentForeign

toHTMLAnchorElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLAnchorElement))
toHTMLAnchorElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLAnchorElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLAreaElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLAreaElement))
toHTMLAreaElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLAreaElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLAudioElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLAudioElement))
toHTMLAudioElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLAudioElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLBRElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLBRElement))
toHTMLBRElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLBRElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLBaseElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLBaseElement))
toHTMLBaseElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLBaseElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLBodyElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLBodyElement))
toHTMLBodyElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLBodyElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLButtonElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLButtonElement))
toHTMLButtonElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof nElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLCanvasElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLCanvasElement))
toHTMLCanvasElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLCanvasElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLDListElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLDListElement))
toHTMLDListElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLDListElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLDataElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLDataElement))
toHTMLDataElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLDataElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLDataListElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLDataListElement))
toHTMLDataListElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLDataListElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLDivElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLDivElement))
toHTMLDivElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLDivElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLDocument :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLDocument))
toHTMLDocument h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLDocument" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLElement))
toHTMLElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLEmbedElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLEmbedElement))
toHTMLEmbedElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLEmbedElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLFieldSetElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLFieldSetElement))
toHTMLFieldSetElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLFieldSetElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLFormElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLFormElement))
toHTMLFormElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLFormElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLHRElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLHRElement))
toHTMLHRElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLHRElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLHeadElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLHeadElement))
toHTMLHeadElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLHeadElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLHeadingElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLHeadingElement))
toHTMLHeadingElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLHeadingElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLIFrameElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLIFrameElement))
toHTMLIFrameElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLIFrameElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLImageElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLImageElement))
toHTMLImageElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLImageElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLInputElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLInputElement))
toHTMLInputElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLInputElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLKeygenElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLKeygenElement))
toHTMLKeygenElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLKeygenElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLLIElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLLIElement))
toHTMLLIElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLLIElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLLabelElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLLabelElement))
toHTMLLabelElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLLabelElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLLegendElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLLegendElement))
toHTMLLegendElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLLegendElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLLinkElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLLinkElement))
toHTMLLinkElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLLinkElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLMapElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLMapElement))
toHTMLMapElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLMapElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLMediaElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLMediaElement))
toHTMLMediaElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLMediaElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLMetaElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLMetaElement))
toHTMLMetaElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLMetaElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLMeterElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLMeterElement))
toHTMLMeterElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLMeterElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLModElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLModElement))
toHTMLModElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLModElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLOListElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLOListElement))
toHTMLOListElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLOListElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLObjectElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLObjectElement))
toHTMLObjectElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLObjectElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLOptGroupElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLOptGroupElement))
toHTMLOptGroupElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLOptGroupElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLOptionElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLOptionElement))
toHTMLOptionElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLOptionElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLOutputElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLOutputElement))
toHTMLOutputElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLOutputElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLParagraphElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLParagraphElement))
toHTMLParagraphElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLParagraphElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLParamElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLParamElement))
toHTMLParamElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLParamElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLPreElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLPreElement))
toHTMLPreElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLPreElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLProgressElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLProgressElement))
toHTMLProgressElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLProgressElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLQuoteElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLQuoteElement))
toHTMLQuoteElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLQuoteElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLScriptElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLScriptElement))
toHTMLScriptElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLScriptElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLSelectElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLSelectElement))
toHTMLSelectElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLSelectElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLSourceElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLSourceElement))
toHTMLSourceElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLSourceElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLSpanElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLSpanElement))
toHTMLSpanElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLSpanElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLStyleElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLStyleElement))
toHTMLStyleElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLStyleElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableCaptionElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableCaptionElement))
toHTMLTableCaptionElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableCaptionElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableCellElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableCellElement))
toHTMLTableCellElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableCellElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableColElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableColElement))
toHTMLTableColElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableColElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableDataCellElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableDataCellElement))
toHTMLTableDataCellElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableDataCellElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableElement))
toHTMLTableElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableHeaderCellElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableHeaderCellElement))
toHTMLTableHeaderCellElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableHeaderCellElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableRowElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableRowElement))
toHTMLTableRowElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableRowElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTableSectionElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTableSectionElement))
toHTMLTableSectionElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTableSectionElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTemplateElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTemplateElement))
toHTMLTemplateElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTemplateElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTextAreaElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTextAreaElement))
toHTMLTextAreaElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTextAreaElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTimeElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTimeElement))
toHTMLTimeElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTimeElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTitleElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTitleElement))
toHTMLTitleElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTitleElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLTrackElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLTrackElement))
toHTMLTrackElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLTrackElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLUListElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLUListElement))
toHTMLUListElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLUListElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing

toHTMLVideoElement :: forall e. Handle e -> Aff (Maybe (Handle HTML.HTMLVideoElement))
toHTMLVideoElement h = do
  isInstance <- Eval.unsafeRunJs0 @Boolean "e => e instanceof HTMLVideoElement" h
  if isInstance then pure $ Just $ unsafeCoerceHandle h else pure Nothing
