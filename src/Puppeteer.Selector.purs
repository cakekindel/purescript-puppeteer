module Puppeteer.Selector where

import Prelude

import Data.Int as Int
import Data.String as String
import Data.Tuple (Tuple(..))
import Web.HTML (HTMLAnchorElement, HTMLAudioElement, HTMLBRElement, HTMLBodyElement, HTMLButtonElement, HTMLCanvasElement, HTMLDListElement, HTMLDataElement, HTMLDataListElement, HTMLDivElement, HTMLElement, HTMLEmbedElement, HTMLFieldSetElement, HTMLFormElement, HTMLHRElement, HTMLHeadElement, HTMLHeadingElement, HTMLIFrameElement, HTMLImageElement, HTMLInputElement, HTMLLIElement, HTMLLabelElement, HTMLLegendElement, HTMLLinkElement, HTMLMetaElement, HTMLMeterElement, HTMLOListElement, HTMLObjectElement, HTMLOptGroupElement, HTMLOptionElement, HTMLParagraphElement, HTMLPreElement, HTMLProgressElement, HTMLQuoteElement, HTMLScriptElement, HTMLSelectElement, HTMLSourceElement, HTMLSpanElement, HTMLStyleElement, HTMLTableCaptionElement, HTMLTableCellElement, HTMLTableColElement, HTMLTableElement, HTMLTableRowElement, HTMLTableSectionElement, HTMLTemplateElement, HTMLTextAreaElement, HTMLTimeElement, HTMLTitleElement, HTMLTrackElement, HTMLUListElement, HTMLVideoElement)
import Web.HTML.HTMLHtmlElement (HTMLHtmlElement)

class Selector :: Type -> Type -> Constraint
class Selector a e | a -> e where
  toCSS :: a -> String

instance selectorString :: Selector String HTMLElement where
  toCSS = identity

instance selectorArraySel :: Selector s e => Selector (Array s) e where
  toCSS = map toCSS >>> String.joinWith ", "

instance selectorHas :: Selector s e => Selector (Has s) e where
  toCSS (HasId s id) = toCSS s <> "#" <> id
  toCSS (HasClass s cls) = toCSS s <> "." <> cls
  toCSS (HasAttrEqualTo s k v) = toCSS s <> "[" <> show k <> "=" <> show v <> "]"
  toCSS (HasAttrListContaining s k v) = toCSS s <> "[" <> show k <> "~=" <> show v <> "]"
  toCSS (HasAttrStartsWith s k v) = toCSS s <> "[" <> show k <> "^=" <> show v <> "]"
  toCSS (HasAttrEndsWith s k v) = toCSS s <> "[" <> show k <> "$=" <> show v <> "]"
  toCSS (HasAttrContaining s k v) = toCSS s <> "[" <> show k <> "*=" <> show v <> "]"
  toCSS (HasAttr s k) = toCSS s <> "[" <> show k <> "]"

instance selectorSelectorRefine :: Selector s e => Selector (SelectorRefine s) e where
  toCSS (SelectorActive a) = toCSS a <> ":active"
  toCSS (SelectorBlank a) = toCSS a <> ":blank"
  toCSS (SelectorChecked a) = toCSS a <> ":checked"
  toCSS (SelectorDefault a) = toCSS a <> ":default"
  toCSS (SelectorDisabled a) = toCSS a <> ":disabled"
  toCSS (SelectorEmpty a) = toCSS a <> ":empty"
  toCSS (SelectorEnabled a) = toCSS a <> ":enabled"
  toCSS (SelectorFirstChild a) = toCSS a <> ":first-child"
  toCSS (SelectorFirstOfType a) = toCSS a <> ":first-of-type"
  toCSS (SelectorLastChild a) = toCSS a <> ":last-child"
  toCSS (SelectorLastOfType a) = toCSS a <> ":last-of-type"
  toCSS (SelectorNthChild n a) = toCSS a <> ":nth-child(" <> Int.toStringAs Int.decimal n <> ")"
  toCSS (SelectorNthOfType n a) = toCSS a <> ":nth-of-type(" <> Int.toStringAs Int.decimal n <> ")"
  toCSS (SelectorOnlyChild a) = toCSS a <> ":only-child"
  toCSS (SelectorOnlyOfType a) = toCSS a <> ":only-of-type"
  toCSS (SelectorFocus a) = toCSS a <> ":focus"
  toCSS (SelectorHover a) = toCSS a <> ":hover"
  toCSS (SelectorLinkVisited a) = toCSS a <> ":visited"
  toCSS (SelectorLinkUnvisited a) = toCSS a <> ":link"
  toCSS (SelectorOptional a) = toCSS a <> ":optional"
  toCSS (SelectorPlaceholderShown a) = toCSS a <> ":placeholder-shown"
  toCSS (SelectorRequired a) = toCSS a <> ":required"
  toCSS (SelectorRoot a) = toCSS a <> ":root"
  toCSS (SelectorInvalid a) = toCSS a <> ":invalid"
  toCSS (SelectorValid a) = toCSS a <> ":valid"

instance selectorSelectorCombinator :: (Selector a e, Selector b f) => Selector (SelectorCombinator a b) e where
  toCSS (SelectorHas a b) = toCSS a <> ":has(" <> toCSS b <> ")"
  toCSS (SelectorIs a b) = toCSS a <> ":is(" <> toCSS b <> ")"
  toCSS (SelectorNot a b) = toCSS a <> ":not(" <> toCSS b <> ")"
  toCSS (SelectorWhere a b) = toCSS a <> ":where(" <> toCSS b <> ")"

data Has a
  = HasId a String
  | HasClass a String
  | HasAttrEqualTo a String String
  | HasAttrListContaining a String String
  | HasAttrStartsWith a String String
  | HasAttrEndsWith a String String
  | HasAttrContaining a String String
  | HasAttr a String

data SelectorRefine a
  = SelectorActive a
  | SelectorBlank a
  | SelectorChecked a
  | SelectorDefault a
  | SelectorDisabled a
  | SelectorEmpty a
  | SelectorEnabled a
  | SelectorFirstChild a
  | SelectorFirstOfType a
  | SelectorLastChild a
  | SelectorLastOfType a
  | SelectorNthChild Int a
  | SelectorNthOfType Int a
  | SelectorOnlyChild a
  | SelectorOnlyOfType a
  | SelectorFocus a
  | SelectorHover a
  | SelectorLinkVisited a
  | SelectorLinkUnvisited a
  | SelectorOptional a
  | SelectorPlaceholderShown a
  | SelectorRequired a
  | SelectorRoot a
  | SelectorInvalid a
  | SelectorValid a

data SelectorCombinator a b
  --| `a:has(b)`
  = SelectorHas a b
  --| `a:is(b)`
  | SelectorIs a b
  --| `a:not(b)`
  | SelectorNot a b
  --| `a:where(b)`
  | SelectorWhere a b

data Combinator a b
  --| `a > b`
  = Child a b
  --| `a b`
  | Descendant a b
  --| `a ~ b`
  | Sibling a b
  --| `a + b`
  | SiblingImmediate a b

instance selectorCombinator :: (Selector a e, Selector b f) => Selector (Combinator a b) f where
  toCSS (Child a b) = toCSS a <> " > " <> toCSS b
  toCSS (Descendant a b) = toCSS a <> " " <> toCSS b
  toCSS (Sibling a b) = toCSS a <> " ~ " <> toCSS b
  toCSS (SiblingImmediate a b) = toCSS a <> " + " <> toCSS b

--| `*`
data TagWild = TagWild

instance selectorTagWild :: Selector TagWild HTMLElement where
  toCSS _ = "*"

--| No tag
data TagNone = TagNone

instance selectorTagNone :: Selector TagNone HTMLElement where
  toCSS _ = ""

--| HTMLAnchorElement
data TagAnchor = TagAnchor

instance selectorTagAnchor :: Selector TagAnchor HTMLAnchorElement where
  toCSS _ = "a"

--| HTMLAudioElement
data TagAudio = TagAudio

instance selectorTagAudio :: Selector TagAudio HTMLAudioElement where
  toCSS _ = "audio"

--| HTMLBRElement
data TagBR = TagBR

instance selectorTagBR :: Selector TagBR HTMLBRElement where
  toCSS _ = "br"

--| HTMLBodyElement
data TagBody = TagBody

instance selectorTagBody :: Selector TagBody HTMLBodyElement where
  toCSS _ = "body"

--| HTMLButtonElement
data TagButton = TagButton

instance selectorTagButton :: Selector TagButton HTMLButtonElement where
  toCSS _ = "button"

--| HTMLCanvasElement
data TagCanvas = TagCanvas

instance selectorTagCanvas :: Selector TagCanvas HTMLCanvasElement where
  toCSS _ = "canvas"

--| HTMLDListElement
data TagDList = TagDList

instance selectorTagDList :: Selector TagDList HTMLDListElement where
  toCSS _ = "dl"

--| HTMLDataElement
data TagData = TagData

instance selectorTagData :: Selector TagData HTMLDataElement where
  toCSS _ = "data"

--| HTMLDataListElement
data TagDataList = TagDataList

instance selectorTagDataList :: Selector TagDataList HTMLDataListElement where
  toCSS _ = "datalist"

--| HTMLDivElement
data TagDiv = TagDiv

instance selectorTagDiv :: Selector TagDiv HTMLDivElement where
  toCSS _ = "div"

--| HTMLEmbedElement
data TagEmbed = TagEmbed

instance selectorTagEmbed :: Selector TagEmbed HTMLEmbedElement where
  toCSS _ = "embed"

--| HTMLFieldSetElement
data TagFieldSet = TagFieldSet

instance selectorTagFieldSet :: Selector TagFieldSet HTMLFieldSetElement where
  toCSS _ = "fieldset"

--| HTMLFormElement
data TagForm = TagForm

instance selectorTagForm :: Selector TagForm HTMLFormElement where
  toCSS _ = "form"

--| HTMLHRElement
data TagHR = TagHR

instance selectorTagHR :: Selector TagHR HTMLHRElement where
  toCSS _ = "hr"

--| HTMLHeadElement
data TagHead = TagHead

instance selectorTagHead :: Selector TagHead HTMLHeadElement where
  toCSS _ = "head"

--| HTMLHeadingElement
data TagH1 = TagH1

instance selectorTagH1 :: Selector TagH1 HTMLHeadingElement where
  toCSS _ = "h1"

--| HTMLHeadingElement
data TagH2 = TagH2

instance selectorTagH2 :: Selector TagH2 HTMLHeadingElement where
  toCSS _ = "h2"

--| HTMLHeadingElement
data TagH3 = TagH3

instance selectorTagH3 :: Selector TagH3 HTMLHeadingElement where
  toCSS _ = "h3"

--| HTMLHeadingElement
data TagH4 = TagH4

instance selectorTagH4 :: Selector TagH4 HTMLHeadingElement where
  toCSS _ = "h4"

--| HTMLHeadingElement
data TagH5 = TagH5

instance selectorTagH5 :: Selector TagH5 HTMLHeadingElement where
  toCSS _ = "h5"

--| HTMLHeadingElement
data TagH6 = TagH6

instance selectorTagH6 :: Selector TagH6 HTMLHeadingElement where
  toCSS _ = "h6"

--| HTMLHtmlElement
data TagHtml = TagHtml

instance selectorTagHtml :: Selector TagHtml HTMLHtmlElement where
  toCSS _ = "html"

--| HTMLIFrameElement
data TagIFrame = TagIFrame

instance selectorTagIFrame :: Selector TagIFrame HTMLIFrameElement where
  toCSS _ = "iframe"

--| HTMLImageElement
data TagImage = TagImage

instance selectorTagImage :: Selector TagImage HTMLImageElement where
  toCSS _ = "img"

--| HTMLInputElement
data TagInput = TagInput

instance selectorTagInput :: Selector TagInput HTMLInputElement where
  toCSS _ = "input"

--| HTMLLIElement
data TagLI = TagLI

instance selectorTagLI :: Selector TagLI HTMLLIElement where
  toCSS _ = "li"

--| HTMLLabelElement
data TagLabel = TagLabel

instance selectorTagLabel :: Selector TagLabel HTMLLabelElement where
  toCSS _ = "label"

--| HTMLLegendElement
data TagLegend = TagLegend

instance selectorTagLegend :: Selector TagLegend HTMLLegendElement where
  toCSS _ = "legend"

--| HTMLLinkElement
data TagLink = TagLink

instance selectorTagLink :: Selector TagLink HTMLLinkElement where
  toCSS _ = "link"

--| HTMLMetaElement
data TagMeta = TagMeta

instance selectorTagMeta :: Selector TagMeta HTMLMetaElement where
  toCSS _ = "meta"

--| HTMLMeterElement
data TagMeter = TagMeter

instance selectorTagMeter :: Selector TagMeter HTMLMeterElement where
  toCSS _ = "meter"

--| HTMLOListElement
data TagOList = TagOList

instance selectorTagOList :: Selector TagOList HTMLOListElement where
  toCSS _ = "ol"

--| HTMLObjectElement
data TagObject = TagObject

instance selectorTagObject :: Selector TagObject HTMLObjectElement where
  toCSS _ = "object"

--| HTMLOptGroupElement
data TagOptGroup = TagOptGroup

instance selectorTagOptGroup :: Selector TagOptGroup HTMLOptGroupElement where
  toCSS _ = "optgroup"

--| HTMLOptionElement
data TagOption = TagOption

instance selectorTagOption :: Selector TagOption HTMLOptionElement where
  toCSS _ = "option"

--| HTMLParagraphElement
data TagParagraph = TagParagraph

instance selectorTagParagraph :: Selector TagParagraph HTMLParagraphElement where
  toCSS _ = "p"

--| HTMLPreElement
data TagPre = TagPre

instance selectorTagPre :: Selector TagPre HTMLPreElement where
  toCSS _ = "pre"

--| HTMLProgressElement
data TagProgress = TagProgress

instance selectorTagProgress :: Selector TagProgress HTMLProgressElement where
  toCSS _ = "progress"

--| HTMLQuoteElement
data TagBlockquote = TagBlockquote

instance selectorTagBlockquote :: Selector TagBlockquote HTMLQuoteElement where
  toCSS _ = "blockquote"

--| HTMLQuoteElement
data TagQ = TagQ

instance selectorTagQ :: Selector TagQ HTMLQuoteElement where
  toCSS _ = "q"

--| HTMLScriptElement
data TagScript = TagScript

instance selectorTagScript :: Selector TagScript HTMLScriptElement where
  toCSS _ = "script"

--| HTMLSelectElement
data TagSelect = TagSelect

instance selectorTagSelect :: Selector TagSelect HTMLSelectElement where
  toCSS _ = "select"

--| HTMLSourceElement
data TagSource = TagSource

instance selectorTagSource :: Selector TagSource HTMLSourceElement where
  toCSS _ = "source"

--| HTMLSpanElement
data TagSpan = TagSpan

instance selectorTagSpan :: Selector TagSpan HTMLSpanElement where
  toCSS _ = "span"

--| HTMLStyleElement
data TagStyle = TagStyle

instance selectorTagStyle :: Selector TagStyle HTMLStyleElement where
  toCSS _ = "style"

--| HTMLTableCaptionElement
data TagTableCaption = TagTableCaption

instance selectorTagTableCaption :: Selector TagTableCaption HTMLTableCaptionElement where
  toCSS _ = "caption"

--| HTMLTableCellElement
data TagTd = TagTd

instance selectorTagTd :: Selector TagTd HTMLTableCellElement where
  toCSS _ = "td"

--| HTMLTableCellElement
data TagTh = TagTh

instance selectorTagTh :: Selector TagTh HTMLTableCellElement where
  toCSS _ = "th"

--| HTMLTableColElement
data TagTableCol = TagTableCol

instance selectorTagTableCol :: Selector TagTableCol HTMLTableColElement where
  toCSS _ = "col"

--| HTMLTableColElement
data TagTableColGroup = TagTableColGroup

instance selectorTagTableColGroup :: Selector TagTableColGroup HTMLTableColElement where
  toCSS _ = "colgroup"

--| HTMLTableElement
data TagTable = TagTable

instance selectorTagTable :: Selector TagTable HTMLTableElement where
  toCSS _ = "table"

--| HTMLTableRowElement
data TagTableRow = TagTableRow

instance selectorTagTableRow :: Selector TagTableRow HTMLTableRowElement where
  toCSS _ = "tr"

--| HTMLTableSectionElement
data TagTBody = TagTBody

instance selectorTagTBody :: Selector TagTBody HTMLTableSectionElement where
  toCSS _ = "tbody"

--| HTMLTableSectionElement
data TagTFoot = TagTFoot

instance selectorTagTFoot :: Selector TagTFoot HTMLTableSectionElement where
  toCSS _ = "tfoot"

--| HTMLTemplateElement
data TagTemplate = TagTemplate

instance selectorTagTemplate :: Selector TagTemplate HTMLTemplateElement where
  toCSS _ = "template"

--| HTMLTextAreaElement
data TagTextArea = TagTextArea

instance selectorTagTextArea :: Selector TagTextArea HTMLTextAreaElement where
  toCSS _ = "textarea"

--| HTMLTimeElement
data TagTime = TagTime

instance selectorTagTime :: Selector TagTime HTMLTimeElement where
  toCSS _ = "time"

--| HTMLTitleElement
data TagTitle = TagTitle

instance selectorTagTitle :: Selector TagTitle HTMLTitleElement where
  toCSS _ = "title"

--| HTMLTrackElement
data TagTrack = TagTrack

instance selectorTagTrack :: Selector TagTrack HTMLTrackElement where
  toCSS _ = "track"

--| HTMLUListElement
data TagUList = TagUList

instance selectorTagUList :: Selector TagUList HTMLUListElement where
  toCSS _ = "ul"

--| HTMLVideoElement
data TagVideo = TagVideo

instance selectorTagVideo :: Selector TagVideo HTMLVideoElement where
  toCSS _ = "video"

wild :: TagWild
wild = TagWild

none :: TagNone
none = TagNone

anchor :: TagAnchor
anchor = TagAnchor

audio :: TagAudio
audio = TagAudio

br :: TagBR
br = TagBR

body :: TagBody
body = TagBody

button :: TagButton
button = TagButton

canvas :: TagCanvas
canvas = TagCanvas

dl :: TagDList
dl = TagDList

data_ :: TagData
data_ = TagData

datalist :: TagDataList
datalist = TagDataList

div :: TagDiv
div = TagDiv

embed :: TagEmbed
embed = TagEmbed

fieldset :: TagFieldSet
fieldset = TagFieldSet

form :: TagForm
form = TagForm

hr :: TagHR
hr = TagHR

head :: TagHead
head = TagHead

h1 :: TagH1
h1 = TagH1

h2 :: TagH2
h2 = TagH2

h3 :: TagH3
h3 = TagH3

h4 :: TagH4
h4 = TagH4

h5 :: TagH5
h5 = TagH5

h6 :: TagH6
h6 = TagH6

html :: TagHtml
html = TagHtml

iframe :: TagIFrame
iframe = TagIFrame

img :: TagImage
img = TagImage

input :: TagInput
input = TagInput

li :: TagLI
li = TagLI

label :: TagLabel
label = TagLabel

legend :: TagLegend
legend = TagLegend

link :: TagLink
link = TagLink

meta :: TagMeta
meta = TagMeta

meter :: TagMeter
meter = TagMeter

ol :: TagOList
ol = TagOList

object :: TagObject
object = TagObject

optgroup :: TagOptGroup
optgroup = TagOptGroup

option :: TagOption
option = TagOption

p :: TagParagraph
p = TagParagraph

pre :: TagPre
pre = TagPre

progress :: TagProgress
progress = TagProgress

blockquote :: TagBlockquote
blockquote = TagBlockquote

q :: TagQ
q = TagQ

script :: TagScript
script = TagScript

select :: TagSelect
select = TagSelect

source :: TagSource
source = TagSource

span :: TagSpan
span = TagSpan

style :: TagStyle
style = TagStyle

caption :: TagTableCaption
caption = TagTableCaption

td :: TagTd
td = TagTd

th :: TagTh
th = TagTh

col :: TagTableCol
col = TagTableCol

colgroup :: TagTableColGroup
colgroup = TagTableColGroup

table :: TagTable
table = TagTable

tr :: TagTableRow
tr = TagTableRow

tbody :: TagTBody
tbody = TagTBody

tfoot :: TagTFoot
tfoot = TagTFoot

template :: TagTemplate
template = TagTemplate

textarea :: TagTextArea
textarea = TagTextArea

time :: TagTime
time = TagTime

title :: TagTitle
title = TagTitle

track :: TagTrack
track = TagTrack

ul :: TagUList
ul = TagUList

video :: TagVideo
video = TagVideo

isChildOf :: forall a b. b -> a -> Combinator a b
isChildOf b a = Child a b

isDescendantOf :: forall a b. b -> a -> Combinator a b
isDescendantOf b a = Descendant a b

isSiblingOf :: forall a b. b -> a -> Combinator a b
isSiblingOf b a = Sibling a b

isImmediateSiblingOf :: forall a b. b -> a -> Combinator a b
isImmediateSiblingOf b a = SiblingImmediate a b

not :: forall a b. a -> b -> SelectorCombinator a b
not = SelectorNot

is :: forall a b. a -> b -> SelectorCombinator a b
is = SelectorIs

has :: forall a b. a -> b -> SelectorCombinator a b
has = SelectorHas

where_ :: forall a b. a -> b -> SelectorCombinator a b
where_ = SelectorWhere

active :: forall a. a -> SelectorRefine a
active = SelectorActive

blank :: forall a. a -> SelectorRefine a
blank = SelectorBlank

checked :: forall a. a -> SelectorRefine a
checked = SelectorChecked

default :: forall a. a -> SelectorRefine a
default = SelectorDefault

disabled :: forall a. a -> SelectorRefine a
disabled = SelectorDisabled

empty :: forall a. a -> SelectorRefine a
empty = SelectorEmpty

enabled :: forall a. a -> SelectorRefine a
enabled = SelectorEnabled

firstChild :: forall a. a -> SelectorRefine a
firstChild = SelectorFirstChild

firstOfType :: forall a. a -> SelectorRefine a
firstOfType = SelectorFirstOfType

lastChild :: forall a. a -> SelectorRefine a
lastChild = SelectorLastChild

lastOfType :: forall a. a -> SelectorRefine a
lastOfType = SelectorLastOfType

nthChild :: forall a. Int -> a -> SelectorRefine a
nthChild = SelectorNthChild

nthOfType :: forall a. Int -> a -> SelectorRefine a
nthOfType = SelectorNthOfType

onlyChild :: forall a. a -> SelectorRefine a
onlyChild = SelectorOnlyChild

onlyOfType :: forall a. a -> SelectorRefine a
onlyOfType = SelectorOnlyOfType

focus :: forall a. a -> SelectorRefine a
focus = SelectorFocus

hover :: forall a. a -> SelectorRefine a
hover = SelectorHover

linkVisited :: forall a. a -> SelectorRefine a
linkVisited = SelectorLinkVisited

linkUnvisited :: forall a. a -> SelectorRefine a
linkUnvisited = SelectorLinkUnvisited

optional :: forall a. a -> SelectorRefine a
optional = SelectorOptional

placeholderShown :: forall a. a -> SelectorRefine a
placeholderShown = SelectorPlaceholderShown

required :: forall a. a -> SelectorRefine a
required = SelectorRequired

root :: forall a. a -> SelectorRefine a
root = SelectorRoot

invalid :: forall a. a -> SelectorRefine a
invalid = SelectorInvalid

valid :: forall a. a -> SelectorRefine a
valid = SelectorValid

hasId :: forall a. a -> String -> Has a
hasId = HasId

hasClass :: forall a. a -> String -> Has a
hasClass = HasClass

hasAttrEqualTo :: forall a. a -> Tuple String String -> Has a
hasAttrEqualTo a (Tuple b c) = HasAttrEqualTo a b c

hasAttrListContaining :: forall a. a -> Tuple String String -> Has a
hasAttrListContaining a (Tuple b c) = HasAttrListContaining a b c

hasAttrStartsWith :: forall a. a -> Tuple String String -> Has a
hasAttrStartsWith a (Tuple b c) = HasAttrStartsWith a b c

hasAttrEndsWith :: forall a. a -> Tuple String String -> Has a
hasAttrEndsWith a (Tuple b c) = HasAttrEndsWith a b c

hasAttrContaining :: forall a. a -> Tuple String String -> Has a
hasAttrContaining a (Tuple b c) = HasAttrContaining a b c

hasAttr :: forall a. a -> String -> Has a
hasAttr a b = HasAttr a b
