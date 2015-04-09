-- | This module defines helper functions for creating HTML attributes.

module Thermite.Html.Attributes where
    
import Thermite.Types
import Thermite.Internal

accept :: forall action. String -> Attr action
accept = unsafeAttribute "accept"

acceptCharset :: forall action. String -> Attr action
acceptCharset = unsafeAttribute "acceptCharset"

accessKey :: forall action. String -> Attr action
accessKey = unsafeAttribute "accessKey"

action :: forall action. String -> Attr action
action = unsafeAttribute "action"

allowFullScreen :: forall action. String -> Attr action
allowFullScreen = unsafeAttribute "allowFullScreen"

allowTransparency :: forall action. String -> Attr action
allowTransparency = unsafeAttribute "allowTransparency"

alt :: forall action. String -> Attr action
alt = unsafeAttribute "alt"

async :: forall action. String -> Attr action
async = unsafeAttribute "async"

autoComplete :: forall action. String -> Attr action
autoComplete = unsafeAttribute "autoComplete"

autoFocus :: forall action. Boolean -> Attr action
autoFocus = unsafeAttribute "autoFocus"

autoPlay :: forall action. String -> Attr action
autoPlay = unsafeAttribute "autoPlay"

cellPadding :: forall action. String -> Attr action
cellPadding = unsafeAttribute "cellPadding"

cellSpacing :: forall action. String -> Attr action
cellSpacing = unsafeAttribute "cellSpacing"

charSet :: forall action. String -> Attr action
charSet = unsafeAttribute "charSet"

checked :: forall action. String -> Attr action
checked = unsafeAttribute "checked"

classID :: forall action. String -> Attr action
classID = unsafeAttribute "classID"

className :: forall action. String -> Attr action
className = unsafeAttribute "className"

cols :: forall action. String -> Attr action
cols = unsafeAttribute "cols"

colSpan :: forall action. String -> Attr action
colSpan = unsafeAttribute "colSpan"

content :: forall action. String -> Attr action
content = unsafeAttribute "content"

contentEditable :: forall action. String -> Attr action
contentEditable = unsafeAttribute "contentEditable"

contextMenu :: forall action. String -> Attr action
contextMenu = unsafeAttribute "contextMenu"

controls :: forall action. String -> Attr action
controls = unsafeAttribute "controls"

coords :: forall action. String -> Attr action
coords = unsafeAttribute "coords"

crossOrigin :: forall action. String -> Attr action
crossOrigin = unsafeAttribute "crossOrigin"

dateTime :: forall action. String -> Attr action
dateTime = unsafeAttribute "dateTime"

defer :: forall action. String -> Attr action
defer = unsafeAttribute "defer"

dir :: forall action. String -> Attr action
dir = unsafeAttribute "dir"

disabled :: forall action. Boolean -> Attr action
disabled = unsafeAttribute "disabled"

download :: forall action. String -> Attr action
download = unsafeAttribute "download"

draggable :: forall action. String -> Attr action
draggable = unsafeAttribute "draggable"

encType :: forall action. String -> Attr action
encType = unsafeAttribute "encType"

form :: forall action. String -> Attr action
form = unsafeAttribute "form"

formAction :: forall action. String -> Attr action
formAction = unsafeAttribute "formAction"

formEncType :: forall action. String -> Attr action
formEncType = unsafeAttribute "formEncType"

formMethod :: forall action. String -> Attr action
formMethod = unsafeAttribute "formMethod"

formNoValidate :: forall action. String -> Attr action
formNoValidate = unsafeAttribute "formNoValidate"

formTarget :: forall action. String -> Attr action
formTarget = unsafeAttribute "formTarget"

frameBorder :: forall action. String -> Attr action
frameBorder = unsafeAttribute "frameBorder"

height :: forall action. String -> Attr action
height = unsafeAttribute "height"

hidden :: forall action. String -> Attr action
hidden = unsafeAttribute "hidden"

href :: forall action. String -> Attr action
href = unsafeAttribute "href"

hrefLang :: forall action. String -> Attr action
hrefLang = unsafeAttribute "hrefLang"

htmlFor :: forall action. String -> Attr action
htmlFor = unsafeAttribute "htmlFor"

httpEquiv :: forall action. String -> Attr action
httpEquiv = unsafeAttribute "httpEquiv"

icon :: forall action. String -> Attr action
icon = unsafeAttribute "icon"

_id :: forall action. String -> Attr action
_id = unsafeAttribute "id"

key :: forall action. String -> Attr action
key = unsafeAttribute "key"

label :: forall action. String -> Attr action
label = unsafeAttribute "label"

lang :: forall action. String -> Attr action
lang = unsafeAttribute "lang"

list :: forall action. String -> Attr action
list = unsafeAttribute "list"

loop :: forall action. String -> Attr action
loop = unsafeAttribute "loop"

manifest :: forall action. String -> Attr action
manifest = unsafeAttribute "manifest"

marginHeight :: forall action. String -> Attr action
marginHeight = unsafeAttribute "marginHeight"

marginWidth :: forall action. String -> Attr action
marginWidth = unsafeAttribute "marginWidth"

max :: forall action. String -> Attr action
max = unsafeAttribute "max"

maxLength :: forall action. String -> Attr action
maxLength = unsafeAttribute "maxLength"

media :: forall action. String -> Attr action
media = unsafeAttribute "media"

mediaGroup :: forall action. String -> Attr action
mediaGroup = unsafeAttribute "mediaGroup"

method :: forall action. String -> Attr action
method = unsafeAttribute "method"

min :: forall action. String -> Attr action
min = unsafeAttribute "min"

multiple :: forall action. String -> Attr action
multiple = unsafeAttribute "multiple"

muted :: forall action. String -> Attr action
muted = unsafeAttribute "muted"

name :: forall action. String -> Attr action
name = unsafeAttribute "name"

noValidate :: forall action. String -> Attr action
noValidate = unsafeAttribute "noValidate"

open :: forall action. String -> Attr action
open = unsafeAttribute "open"

pattern :: forall action. String -> Attr action
pattern = unsafeAttribute "pattern"

placeholder :: forall action. String -> Attr action
placeholder = unsafeAttribute "placeholder"

poster :: forall action. String -> Attr action
poster = unsafeAttribute "poster"

preload :: forall action. String -> Attr action
preload = unsafeAttribute "preload"

radioGroup :: forall action. String -> Attr action
radioGroup = unsafeAttribute "radioGroup"

readOnly :: forall action. String -> Attr action
readOnly = unsafeAttribute "readOnly"

rel :: forall action. String -> Attr action
rel = unsafeAttribute "rel"

required :: forall action. String -> Attr action
required = unsafeAttribute "required"

role :: forall action. String -> Attr action
role = unsafeAttribute "role"

rows :: forall action. String -> Attr action
rows = unsafeAttribute "rows"

rowSpan :: forall action. String -> Attr action
rowSpan = unsafeAttribute "rowSpan"

sandbox :: forall action. String -> Attr action
sandbox = unsafeAttribute "sandbox"

scope :: forall action. String -> Attr action
scope = unsafeAttribute "scope"

scrolling :: forall action. String -> Attr action
scrolling = unsafeAttribute "scrolling"

seamless :: forall action. String -> Attr action
seamless = unsafeAttribute "seamless"

selected :: forall action. String -> Attr action
selected = unsafeAttribute "selected"

shape :: forall action. String -> Attr action
shape = unsafeAttribute "shape"

size :: forall action. String -> Attr action
size = unsafeAttribute "size"

sizes :: forall action. String -> Attr action
sizes = unsafeAttribute "sizes"

span :: forall action. String -> Attr action
span = unsafeAttribute "span"

spellCheck :: forall action. String -> Attr action
spellCheck = unsafeAttribute "spellCheck"

src :: forall action. String -> Attr action
src = unsafeAttribute "src"

srcDoc :: forall action. String -> Attr action
srcDoc = unsafeAttribute "srcDoc"

srcSet :: forall action. String -> Attr action
srcSet = unsafeAttribute "srcSet"

start :: forall action. String -> Attr action
start = unsafeAttribute "start"

step :: forall action. String -> Attr action
step = unsafeAttribute "step"

tabIndex :: forall action. String -> Attr action
tabIndex = unsafeAttribute "tabIndex"

target :: forall action. String -> Attr action
target = unsafeAttribute "target"

title :: forall action. String -> Attr action
title = unsafeAttribute "title"

_type :: forall action. String -> Attr action
_type = unsafeAttribute "type"

useMap :: forall action. String -> Attr action
useMap = unsafeAttribute "useMap"

value :: forall action. String -> Attr action
value = unsafeAttribute "value"

width :: forall action. String -> Attr action
width = unsafeAttribute "width"

wmode :: forall action. String -> Attr action
wmode = unsafeAttribute "wmode"

