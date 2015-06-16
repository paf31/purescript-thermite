-- | This module defines helper functions for creating HTML attributes.

module Thermite.Html.Attributes where
    
import Prelude
    
import Thermite.Types
import Thermite.Internal

accept :: String -> Attr
accept = unsafeAttribute "accept"

acceptCharset :: String -> Attr
acceptCharset = unsafeAttribute "acceptCharset"

accessKey :: String -> Attr
accessKey = unsafeAttribute "accessKey"

action :: String -> Attr
action = unsafeAttribute "action"

allowFullScreen :: String -> Attr
allowFullScreen = unsafeAttribute "allowFullScreen"

allowTransparency :: String -> Attr
allowTransparency = unsafeAttribute "allowTransparency"

alt :: String -> Attr
alt = unsafeAttribute "alt"

async :: String -> Attr
async = unsafeAttribute "async"

autoComplete :: String -> Attr
autoComplete = unsafeAttribute "autoComplete"

autoFocus :: Boolean -> Attr
autoFocus = unsafeAttribute "autoFocus"

autoPlay :: String -> Attr
autoPlay = unsafeAttribute "autoPlay"

cellPadding :: String -> Attr
cellPadding = unsafeAttribute "cellPadding"

cellSpacing :: String -> Attr
cellSpacing = unsafeAttribute "cellSpacing"

charSet :: String -> Attr
charSet = unsafeAttribute "charSet"

checked :: String -> Attr
checked = unsafeAttribute "checked"

classID :: String -> Attr
classID = unsafeAttribute "classID"

className :: String -> Attr
className = unsafeAttribute "className"

cols :: String -> Attr
cols = unsafeAttribute "cols"

colSpan :: String -> Attr
colSpan = unsafeAttribute "colSpan"

content :: String -> Attr
content = unsafeAttribute "content"

contentEditable :: String -> Attr
contentEditable = unsafeAttribute "contentEditable"

contextMenu :: String -> Attr
contextMenu = unsafeAttribute "contextMenu"

controls :: String -> Attr
controls = unsafeAttribute "controls"

coords :: String -> Attr
coords = unsafeAttribute "coords"

crossOrigin :: String -> Attr
crossOrigin = unsafeAttribute "crossOrigin"

dateTime :: String -> Attr
dateTime = unsafeAttribute "dateTime"

defer :: String -> Attr
defer = unsafeAttribute "defer"

dir :: String -> Attr
dir = unsafeAttribute "dir"

disabled :: Boolean -> Attr
disabled = unsafeAttribute "disabled"

download :: String -> Attr
download = unsafeAttribute "download"

draggable :: String -> Attr
draggable = unsafeAttribute "draggable"

encType :: String -> Attr
encType = unsafeAttribute "encType"

form :: String -> Attr
form = unsafeAttribute "form"

formAction :: String -> Attr
formAction = unsafeAttribute "formAction"

formEncType :: String -> Attr
formEncType = unsafeAttribute "formEncType"

formMethod :: String -> Attr
formMethod = unsafeAttribute "formMethod"

formNoValidate :: String -> Attr
formNoValidate = unsafeAttribute "formNoValidate"

formTarget :: String -> Attr
formTarget = unsafeAttribute "formTarget"

frameBorder :: String -> Attr
frameBorder = unsafeAttribute "frameBorder"

height :: String -> Attr
height = unsafeAttribute "height"

hidden :: String -> Attr
hidden = unsafeAttribute "hidden"

href :: String -> Attr
href = unsafeAttribute "href"

hrefLang :: String -> Attr
hrefLang = unsafeAttribute "hrefLang"

htmlFor :: String -> Attr
htmlFor = unsafeAttribute "htmlFor"

httpEquiv :: String -> Attr
httpEquiv = unsafeAttribute "httpEquiv"

icon :: String -> Attr
icon = unsafeAttribute "icon"

_id :: String -> Attr
_id = unsafeAttribute "id"

key :: String -> Attr
key = unsafeAttribute "key"

label :: String -> Attr
label = unsafeAttribute "label"

lang :: String -> Attr
lang = unsafeAttribute "lang"

list :: String -> Attr
list = unsafeAttribute "list"

loop :: String -> Attr
loop = unsafeAttribute "loop"

manifest :: String -> Attr
manifest = unsafeAttribute "manifest"

marginHeight :: String -> Attr
marginHeight = unsafeAttribute "marginHeight"

marginWidth :: String -> Attr
marginWidth = unsafeAttribute "marginWidth"

max :: String -> Attr
max = unsafeAttribute "max"

maxLength :: String -> Attr
maxLength = unsafeAttribute "maxLength"

media :: String -> Attr
media = unsafeAttribute "media"

mediaGroup :: String -> Attr
mediaGroup = unsafeAttribute "mediaGroup"

method :: String -> Attr
method = unsafeAttribute "method"

min :: String -> Attr
min = unsafeAttribute "min"

multiple :: String -> Attr
multiple = unsafeAttribute "multiple"

muted :: String -> Attr
muted = unsafeAttribute "muted"

name :: String -> Attr
name = unsafeAttribute "name"

noValidate :: String -> Attr
noValidate = unsafeAttribute "noValidate"

open :: String -> Attr
open = unsafeAttribute "open"

pattern :: String -> Attr
pattern = unsafeAttribute "pattern"

placeholder :: String -> Attr
placeholder = unsafeAttribute "placeholder"

poster :: String -> Attr
poster = unsafeAttribute "poster"

preload :: String -> Attr
preload = unsafeAttribute "preload"

radioGroup :: String -> Attr
radioGroup = unsafeAttribute "radioGroup"

readOnly :: String -> Attr
readOnly = unsafeAttribute "readOnly"

rel :: String -> Attr
rel = unsafeAttribute "rel"

required :: String -> Attr
required = unsafeAttribute "required"

role :: String -> Attr
role = unsafeAttribute "role"

rows :: String -> Attr
rows = unsafeAttribute "rows"

rowSpan :: String -> Attr
rowSpan = unsafeAttribute "rowSpan"

sandbox :: String -> Attr
sandbox = unsafeAttribute "sandbox"

scope :: String -> Attr
scope = unsafeAttribute "scope"

scrolling :: String -> Attr
scrolling = unsafeAttribute "scrolling"

seamless :: String -> Attr
seamless = unsafeAttribute "seamless"

selected :: String -> Attr
selected = unsafeAttribute "selected"

shape :: String -> Attr
shape = unsafeAttribute "shape"

size :: String -> Attr
size = unsafeAttribute "size"

sizes :: String -> Attr
sizes = unsafeAttribute "sizes"

span :: String -> Attr
span = unsafeAttribute "span"

spellCheck :: String -> Attr
spellCheck = unsafeAttribute "spellCheck"

src :: String -> Attr
src = unsafeAttribute "src"

srcDoc :: String -> Attr
srcDoc = unsafeAttribute "srcDoc"

srcSet :: String -> Attr
srcSet = unsafeAttribute "srcSet"

start :: String -> Attr
start = unsafeAttribute "start"

step :: String -> Attr
step = unsafeAttribute "step"

tabIndex :: String -> Attr
tabIndex = unsafeAttribute "tabIndex"

target :: String -> Attr
target = unsafeAttribute "target"

title :: String -> Attr
title = unsafeAttribute "title"

_type :: String -> Attr
_type = unsafeAttribute "type"

useMap :: String -> Attr
useMap = unsafeAttribute "useMap"

value :: String -> Attr
value = unsafeAttribute "value"

width :: String -> Attr
width = unsafeAttribute "width"

wmode :: String -> Attr
wmode = unsafeAttribute "wmode"

