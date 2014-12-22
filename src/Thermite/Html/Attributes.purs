module Thermite.Html.Attributes where
    
import Thermite.Html
    
foreign import attribute """
  function attribute(attr) {
    return function(value) {
      return [attr, value];
    };
  }   
  """ :: forall action. String -> String -> Prop action
  
accept :: forall action. String -> Prop action
accept = attribute "accept"

acceptCharset :: forall action. String -> Prop action
acceptCharset = attribute "acceptCharset"

accessKey :: forall action. String -> Prop action
accessKey = attribute "accessKey"

action :: forall action. String -> Prop action
action = attribute "action"

allowFullScreen :: forall action. String -> Prop action
allowFullScreen = attribute "allowFullScreen"

allowTransparency :: forall action. String -> Prop action
allowTransparency = attribute "allowTransparency"

alt :: forall action. String -> Prop action
alt = attribute "alt"

async :: forall action. String -> Prop action
async = attribute "async"

autoComplete :: forall action. String -> Prop action
autoComplete = attribute "autoComplete"

autoPlay :: forall action. String -> Prop action
autoPlay = attribute "autoPlay"

cellPadding :: forall action. String -> Prop action
cellPadding = attribute "cellPadding"

cellSpacing :: forall action. String -> Prop action
cellSpacing = attribute "cellSpacing"

charSet :: forall action. String -> Prop action
charSet = attribute "charSet"

checked :: forall action. String -> Prop action
checked = attribute "checked"

classID :: forall action. String -> Prop action
classID = attribute "classID"

className :: forall action. String -> Prop action
className = attribute "className"

cols :: forall action. String -> Prop action
cols = attribute "cols"

colSpan :: forall action. String -> Prop action
colSpan = attribute "colSpan"

content :: forall action. String -> Prop action
content = attribute "content"

contentEditable :: forall action. String -> Prop action
contentEditable = attribute "contentEditable"

contextMenu :: forall action. String -> Prop action
contextMenu = attribute "contextMenu"

controls :: forall action. String -> Prop action
controls = attribute "controls"

coords :: forall action. String -> Prop action
coords = attribute "coords"

crossOrigin :: forall action. String -> Prop action
crossOrigin = attribute "crossOrigin"

dateTime :: forall action. String -> Prop action
dateTime = attribute "dateTime"

defer :: forall action. String -> Prop action
defer = attribute "defer"

dir :: forall action. String -> Prop action
dir = attribute "dir"

disabled :: forall action. String -> Prop action
disabled = attribute "disabled"

download :: forall action. String -> Prop action
download = attribute "download"

draggable :: forall action. String -> Prop action
draggable = attribute "draggable"

encType :: forall action. String -> Prop action
encType = attribute "encType"

form :: forall action. String -> Prop action
form = attribute "form"

formAction :: forall action. String -> Prop action
formAction = attribute "formAction"

formEncType :: forall action. String -> Prop action
formEncType = attribute "formEncType"

formMethod :: forall action. String -> Prop action
formMethod = attribute "formMethod"

formNoValidate :: forall action. String -> Prop action
formNoValidate = attribute "formNoValidate"

formTarget :: forall action. String -> Prop action
formTarget = attribute "formTarget"

frameBorder :: forall action. String -> Prop action
frameBorder = attribute "frameBorder"

height :: forall action. String -> Prop action
height = attribute "height"

hidden :: forall action. String -> Prop action
hidden = attribute "hidden"

href :: forall action. String -> Prop action
href = attribute "href"

hrefLang :: forall action. String -> Prop action
hrefLang = attribute "hrefLang"

htmlFor :: forall action. String -> Prop action
htmlFor = attribute "htmlFor"

httpEquiv :: forall action. String -> Prop action
httpEquiv = attribute "httpEquiv"

icon :: forall action. String -> Prop action
icon = attribute "icon"

_id :: forall action. String -> Prop action
_id = attribute "id"

label :: forall action. String -> Prop action
label = attribute "label"

lang :: forall action. String -> Prop action
lang = attribute "lang"

list :: forall action. String -> Prop action
list = attribute "list"

loop :: forall action. String -> Prop action
loop = attribute "loop"

manifest :: forall action. String -> Prop action
manifest = attribute "manifest"

marginHeight :: forall action. String -> Prop action
marginHeight = attribute "marginHeight"

marginWidth :: forall action. String -> Prop action
marginWidth = attribute "marginWidth"

max :: forall action. String -> Prop action
max = attribute "max"

maxLength :: forall action. String -> Prop action
maxLength = attribute "maxLength"

media :: forall action. String -> Prop action
media = attribute "media"

mediaGroup :: forall action. String -> Prop action
mediaGroup = attribute "mediaGroup"

method :: forall action. String -> Prop action
method = attribute "method"

min :: forall action. String -> Prop action
min = attribute "min"

multiple :: forall action. String -> Prop action
multiple = attribute "multiple"

muted :: forall action. String -> Prop action
muted = attribute "muted"

name :: forall action. String -> Prop action
name = attribute "name"

noValidate :: forall action. String -> Prop action
noValidate = attribute "noValidate"

open :: forall action. String -> Prop action
open = attribute "open"

pattern :: forall action. String -> Prop action
pattern = attribute "pattern"

placeholder :: forall action. String -> Prop action
placeholder = attribute "placeholder"

poster :: forall action. String -> Prop action
poster = attribute "poster"

preload :: forall action. String -> Prop action
preload = attribute "preload"

radioGroup :: forall action. String -> Prop action
radioGroup = attribute "radioGroup"

readOnly :: forall action. String -> Prop action
readOnly = attribute "readOnly"

rel :: forall action. String -> Prop action
rel = attribute "rel"

required :: forall action. String -> Prop action
required = attribute "required"

role :: forall action. String -> Prop action
role = attribute "role"

rows :: forall action. String -> Prop action
rows = attribute "rows"

rowSpan :: forall action. String -> Prop action
rowSpan = attribute "rowSpan"

sandbox :: forall action. String -> Prop action
sandbox = attribute "sandbox"

scope :: forall action. String -> Prop action
scope = attribute "scope"

scrolling :: forall action. String -> Prop action
scrolling = attribute "scrolling"

seamless :: forall action. String -> Prop action
seamless = attribute "seamless"

selected :: forall action. String -> Prop action
selected = attribute "selected"

shape :: forall action. String -> Prop action
shape = attribute "shape"

size :: forall action. String -> Prop action
size = attribute "size"

sizes :: forall action. String -> Prop action
sizes = attribute "sizes"

span :: forall action. String -> Prop action
span = attribute "span"

spellCheck :: forall action. String -> Prop action
spellCheck = attribute "spellCheck"

src :: forall action. String -> Prop action
src = attribute "src"

srcDoc :: forall action. String -> Prop action
srcDoc = attribute "srcDoc"

srcSet :: forall action. String -> Prop action
srcSet = attribute "srcSet"

start :: forall action. String -> Prop action
start = attribute "start"

step :: forall action. String -> Prop action
step = attribute "step"

tabIndex :: forall action. String -> Prop action
tabIndex = attribute "tabIndex"

target :: forall action. String -> Prop action
target = attribute "target"

title :: forall action. String -> Prop action
title = attribute "title"

_type :: forall action. String -> Prop action
_type = attribute "type"

useMap :: forall action. String -> Prop action
useMap = attribute "useMap"

value :: forall action. String -> Prop action
value = attribute "value"

width :: forall action. String -> Prop action
width = attribute "width"

wmode :: forall action. String -> Prop action
wmode = attribute "wmode"

