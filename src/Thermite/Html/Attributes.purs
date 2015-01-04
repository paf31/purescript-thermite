module Thermite.Html.Attributes where
    
import Thermite.Types
import Thermite.Internal

accept :: forall action. String -> Prop action
accept = unsafeAttribute "accept"

acceptCharset :: forall action. String -> Prop action
acceptCharset = unsafeAttribute "acceptCharset"

accessKey :: forall action. String -> Prop action
accessKey = unsafeAttribute "accessKey"

action :: forall action. String -> Prop action
action = unsafeAttribute "action"

allowFullScreen :: forall action. String -> Prop action
allowFullScreen = unsafeAttribute "allowFullScreen"

allowTransparency :: forall action. String -> Prop action
allowTransparency = unsafeAttribute "allowTransparency"

alt :: forall action. String -> Prop action
alt = unsafeAttribute "alt"

async :: forall action. String -> Prop action
async = unsafeAttribute "async"

autoComplete :: forall action. String -> Prop action
autoComplete = unsafeAttribute "autoComplete"

autoFocus :: forall action. Boolean -> Prop action
autoFocus = unsafeAttribute "autoFocus"

autoPlay :: forall action. String -> Prop action
autoPlay = unsafeAttribute "autoPlay"

cellPadding :: forall action. String -> Prop action
cellPadding = unsafeAttribute "cellPadding"

cellSpacing :: forall action. String -> Prop action
cellSpacing = unsafeAttribute "cellSpacing"

charSet :: forall action. String -> Prop action
charSet = unsafeAttribute "charSet"

checked :: forall action. String -> Prop action
checked = unsafeAttribute "checked"

classID :: forall action. String -> Prop action
classID = unsafeAttribute "classID"

className :: forall action. String -> Prop action
className = unsafeAttribute "className"

cols :: forall action. String -> Prop action
cols = unsafeAttribute "cols"

colSpan :: forall action. String -> Prop action
colSpan = unsafeAttribute "colSpan"

content :: forall action. String -> Prop action
content = unsafeAttribute "content"

contentEditable :: forall action. String -> Prop action
contentEditable = unsafeAttribute "contentEditable"

contextMenu :: forall action. String -> Prop action
contextMenu = unsafeAttribute "contextMenu"

controls :: forall action. String -> Prop action
controls = unsafeAttribute "controls"

coords :: forall action. String -> Prop action
coords = unsafeAttribute "coords"

crossOrigin :: forall action. String -> Prop action
crossOrigin = unsafeAttribute "crossOrigin"

dateTime :: forall action. String -> Prop action
dateTime = unsafeAttribute "dateTime"

defer :: forall action. String -> Prop action
defer = unsafeAttribute "defer"

dir :: forall action. String -> Prop action
dir = unsafeAttribute "dir"

disabled :: forall action. String -> Prop action
disabled = unsafeAttribute "disabled"

download :: forall action. String -> Prop action
download = unsafeAttribute "download"

draggable :: forall action. String -> Prop action
draggable = unsafeAttribute "draggable"

encType :: forall action. String -> Prop action
encType = unsafeAttribute "encType"

form :: forall action. String -> Prop action
form = unsafeAttribute "form"

formAction :: forall action. String -> Prop action
formAction = unsafeAttribute "formAction"

formEncType :: forall action. String -> Prop action
formEncType = unsafeAttribute "formEncType"

formMethod :: forall action. String -> Prop action
formMethod = unsafeAttribute "formMethod"

formNoValidate :: forall action. String -> Prop action
formNoValidate = unsafeAttribute "formNoValidate"

formTarget :: forall action. String -> Prop action
formTarget = unsafeAttribute "formTarget"

frameBorder :: forall action. String -> Prop action
frameBorder = unsafeAttribute "frameBorder"

height :: forall action. String -> Prop action
height = unsafeAttribute "height"

hidden :: forall action. String -> Prop action
hidden = unsafeAttribute "hidden"

href :: forall action. String -> Prop action
href = unsafeAttribute "href"

hrefLang :: forall action. String -> Prop action
hrefLang = unsafeAttribute "hrefLang"

htmlFor :: forall action. String -> Prop action
htmlFor = unsafeAttribute "htmlFor"

httpEquiv :: forall action. String -> Prop action
httpEquiv = unsafeAttribute "httpEquiv"

icon :: forall action. String -> Prop action
icon = unsafeAttribute "icon"

_id :: forall action. String -> Prop action
_id = unsafeAttribute "id"

label :: forall action. String -> Prop action
label = unsafeAttribute "label"

lang :: forall action. String -> Prop action
lang = unsafeAttribute "lang"

list :: forall action. String -> Prop action
list = unsafeAttribute "list"

loop :: forall action. String -> Prop action
loop = unsafeAttribute "loop"

manifest :: forall action. String -> Prop action
manifest = unsafeAttribute "manifest"

marginHeight :: forall action. String -> Prop action
marginHeight = unsafeAttribute "marginHeight"

marginWidth :: forall action. String -> Prop action
marginWidth = unsafeAttribute "marginWidth"

max :: forall action. String -> Prop action
max = unsafeAttribute "max"

maxLength :: forall action. String -> Prop action
maxLength = unsafeAttribute "maxLength"

media :: forall action. String -> Prop action
media = unsafeAttribute "media"

mediaGroup :: forall action. String -> Prop action
mediaGroup = unsafeAttribute "mediaGroup"

method :: forall action. String -> Prop action
method = unsafeAttribute "method"

min :: forall action. String -> Prop action
min = unsafeAttribute "min"

multiple :: forall action. String -> Prop action
multiple = unsafeAttribute "multiple"

muted :: forall action. String -> Prop action
muted = unsafeAttribute "muted"

name :: forall action. String -> Prop action
name = unsafeAttribute "name"

noValidate :: forall action. String -> Prop action
noValidate = unsafeAttribute "noValidate"

open :: forall action. String -> Prop action
open = unsafeAttribute "open"

pattern :: forall action. String -> Prop action
pattern = unsafeAttribute "pattern"

placeholder :: forall action. String -> Prop action
placeholder = unsafeAttribute "placeholder"

poster :: forall action. String -> Prop action
poster = unsafeAttribute "poster"

preload :: forall action. String -> Prop action
preload = unsafeAttribute "preload"

radioGroup :: forall action. String -> Prop action
radioGroup = unsafeAttribute "radioGroup"

readOnly :: forall action. String -> Prop action
readOnly = unsafeAttribute "readOnly"

rel :: forall action. String -> Prop action
rel = unsafeAttribute "rel"

required :: forall action. String -> Prop action
required = unsafeAttribute "required"

role :: forall action. String -> Prop action
role = unsafeAttribute "role"

rows :: forall action. String -> Prop action
rows = unsafeAttribute "rows"

rowSpan :: forall action. String -> Prop action
rowSpan = unsafeAttribute "rowSpan"

sandbox :: forall action. String -> Prop action
sandbox = unsafeAttribute "sandbox"

scope :: forall action. String -> Prop action
scope = unsafeAttribute "scope"

scrolling :: forall action. String -> Prop action
scrolling = unsafeAttribute "scrolling"

seamless :: forall action. String -> Prop action
seamless = unsafeAttribute "seamless"

selected :: forall action. String -> Prop action
selected = unsafeAttribute "selected"

shape :: forall action. String -> Prop action
shape = unsafeAttribute "shape"

size :: forall action. String -> Prop action
size = unsafeAttribute "size"

sizes :: forall action. String -> Prop action
sizes = unsafeAttribute "sizes"

span :: forall action. String -> Prop action
span = unsafeAttribute "span"

spellCheck :: forall action. String -> Prop action
spellCheck = unsafeAttribute "spellCheck"

src :: forall action. String -> Prop action
src = unsafeAttribute "src"

srcDoc :: forall action. String -> Prop action
srcDoc = unsafeAttribute "srcDoc"

srcSet :: forall action. String -> Prop action
srcSet = unsafeAttribute "srcSet"

start :: forall action. String -> Prop action
start = unsafeAttribute "start"

step :: forall action. String -> Prop action
step = unsafeAttribute "step"

tabIndex :: forall action. String -> Prop action
tabIndex = unsafeAttribute "tabIndex"

target :: forall action. String -> Prop action
target = unsafeAttribute "target"

title :: forall action. String -> Prop action
title = unsafeAttribute "title"

_type :: forall action. String -> Prop action
_type = unsafeAttribute "type"

useMap :: forall action. String -> Prop action
useMap = unsafeAttribute "useMap"

value :: forall action. String -> Prop action
value = unsafeAttribute "value"

width :: forall action. String -> Prop action
width = unsafeAttribute "width"

wmode :: forall action. String -> Prop action
wmode = unsafeAttribute "wmode"

