# Module Documentation

## Module Thermite

#### `simpleSpec`

``` purescript
simpleSpec :: forall m state props action. state -> PerformAction props action m -> Render state props action -> Spec m state props action
```


#### `componentWillMount`

``` purescript
componentWillMount :: forall m state props action. action -> Spec m state props action -> Spec m state props action
```


#### `displayName`

``` purescript
displayName :: forall m state props action. String -> Spec m state props action -> Spec m state props action
```


#### `createClass`

``` purescript
createClass :: forall eff state props action. Spec (Action eff state) state props action -> ComponentClass props eff
```


#### `render`

``` purescript
render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
```


#### `renderTo`

``` purescript
renderTo :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
```



## Module Thermite.Action

#### `functorActionF`

``` purescript
instance functorActionF :: Functor (ActionF eff state)
```


#### `Action`

``` purescript
data Action eff state a
```


#### `runAction`

``` purescript
runAction :: forall eff state props action a. Context state props action -> Action eff state a -> Eff eff Unit
```


#### `getState`

``` purescript
getState :: forall eff state. Action eff state state
```


#### `setState`

``` purescript
setState :: forall eff state. state -> Action eff state Unit
```


#### `modifyState`

``` purescript
modifyState :: forall eff state. (state -> state) -> Action eff state Unit
```


#### `async`

``` purescript
async :: forall eff state a. ((a -> Eff eff Unit) -> Eff eff Unit) -> Action eff state a
```


#### `sync`

``` purescript
sync :: forall eff state a. Eff eff a -> Action eff state a
```


#### `asyncSetState`

``` purescript
asyncSetState :: forall eff state. ((state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state Unit
```


#### `functorAction`

``` purescript
instance functorAction :: Functor (Action eff state)
```


#### `applyAction`

``` purescript
instance applyAction :: Apply (Action eff state)
```


#### `applicativeAction`

``` purescript
instance applicativeAction :: Applicative (Action eff state)
```


#### `bindAction`

``` purescript
instance bindAction :: Bind (Action eff state)
```


#### `monadAction`

``` purescript
instance monadAction :: Monad (Action eff state)
```



## Module Thermite.Events

#### `ClipboardEvent`

``` purescript
data ClipboardEvent :: *
```


#### `onCopy`

``` purescript
onCopy :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action
```


#### `onCut`

``` purescript
onCut :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action
```


#### `onPaste`

``` purescript
onPaste :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action
```


#### `KeyboardEvent`

``` purescript
data KeyboardEvent :: *
```


#### `onKeyDown`

``` purescript
onKeyDown :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action
```


#### `onKeyPress`

``` purescript
onKeyPress :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action
```


#### `onKeyUp`

``` purescript
onKeyUp :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action
```


#### `FocusEvent`

``` purescript
data FocusEvent :: *
```


#### `onFocus`

``` purescript
onFocus :: forall state props action. Context state props action -> (FocusEvent -> action) -> Prop action
```


#### `onBlur`

``` purescript
onBlur :: forall state props action. Context state props action -> (FocusEvent -> action) -> Prop action
```


#### `FormEvent`

``` purescript
data FormEvent :: *
```


#### `onChange`

``` purescript
onChange :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action
```


#### `onInput`

``` purescript
onInput :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action
```


#### `onSubmit`

``` purescript
onSubmit :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action
```


#### `MouseEvent`

``` purescript
data MouseEvent :: *
```


#### `onClick`

``` purescript
onClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDoubleClick`

``` purescript
onDoubleClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDrag`

``` purescript
onDrag :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDragEnd`

``` purescript
onDragEnd :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDragEnter`

``` purescript
onDragEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDragExit`

``` purescript
onDragExit :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDragLeave`

``` purescript
onDragLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDragOver`

``` purescript
onDragOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDragStart`

``` purescript
onDragStart :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onDrop`

``` purescript
onDrop :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onMouseDown`

``` purescript
onMouseDown :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onMouseEnter`

``` purescript
onMouseEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onMouseLeave`

``` purescript
onMouseLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onMouseMove`

``` purescript
onMouseMove :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onMouseOut`

``` purescript
onMouseOut :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onMouseOver`

``` purescript
onMouseOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `onMouseUp`

``` purescript
onMouseUp :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
```


#### `TouchEvent`

``` purescript
data TouchEvent :: *
```


#### `onTouchCancel`

``` purescript
onTouchCancel :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
```


#### `onTouchEnd`

``` purescript
onTouchEnd :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
```


#### `onTouchMove`

``` purescript
onTouchMove :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
```


#### `onTouchStart`

``` purescript
onTouchStart :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
```


#### `UIEvent`

``` purescript
data UIEvent :: *
```


#### `onScroll`

``` purescript
onScroll :: forall state props action. Context state props action -> (UIEvent -> action) -> Prop action
```


#### `WheelEvent`

``` purescript
data WheelEvent :: *
```


#### `onWheel`

``` purescript
onWheel :: forall state props action. Context state props action -> (WheelEvent -> action) -> Prop action
```



## Module Thermite.Html

#### `text`

``` purescript
text :: forall action. String -> Html action
```


#### `createElement`

``` purescript
createElement :: forall action. String -> Props action -> [Html action] -> Html action
```



## Module Thermite.Html.Attributes

#### `accept`

``` purescript
accept :: forall action. String -> Prop action
```


#### `acceptCharset`

``` purescript
acceptCharset :: forall action. String -> Prop action
```


#### `accessKey`

``` purescript
accessKey :: forall action. String -> Prop action
```


#### `action`

``` purescript
action :: forall action. String -> Prop action
```


#### `allowFullScreen`

``` purescript
allowFullScreen :: forall action. String -> Prop action
```


#### `allowTransparency`

``` purescript
allowTransparency :: forall action. String -> Prop action
```


#### `alt`

``` purescript
alt :: forall action. String -> Prop action
```


#### `async`

``` purescript
async :: forall action. String -> Prop action
```


#### `autoComplete`

``` purescript
autoComplete :: forall action. String -> Prop action
```


#### `autoFocus`

``` purescript
autoFocus :: forall action. Boolean -> Prop action
```


#### `autoPlay`

``` purescript
autoPlay :: forall action. String -> Prop action
```


#### `cellPadding`

``` purescript
cellPadding :: forall action. String -> Prop action
```


#### `cellSpacing`

``` purescript
cellSpacing :: forall action. String -> Prop action
```


#### `charSet`

``` purescript
charSet :: forall action. String -> Prop action
```


#### `checked`

``` purescript
checked :: forall action. String -> Prop action
```


#### `classID`

``` purescript
classID :: forall action. String -> Prop action
```


#### `className`

``` purescript
className :: forall action. String -> Prop action
```


#### `cols`

``` purescript
cols :: forall action. String -> Prop action
```


#### `colSpan`

``` purescript
colSpan :: forall action. String -> Prop action
```


#### `content`

``` purescript
content :: forall action. String -> Prop action
```


#### `contentEditable`

``` purescript
contentEditable :: forall action. String -> Prop action
```


#### `contextMenu`

``` purescript
contextMenu :: forall action. String -> Prop action
```


#### `controls`

``` purescript
controls :: forall action. String -> Prop action
```


#### `coords`

``` purescript
coords :: forall action. String -> Prop action
```


#### `crossOrigin`

``` purescript
crossOrigin :: forall action. String -> Prop action
```


#### `dateTime`

``` purescript
dateTime :: forall action. String -> Prop action
```


#### `defer`

``` purescript
defer :: forall action. String -> Prop action
```


#### `dir`

``` purescript
dir :: forall action. String -> Prop action
```


#### `disabled`

``` purescript
disabled :: forall action. String -> Prop action
```


#### `download`

``` purescript
download :: forall action. String -> Prop action
```


#### `draggable`

``` purescript
draggable :: forall action. String -> Prop action
```


#### `encType`

``` purescript
encType :: forall action. String -> Prop action
```


#### `form`

``` purescript
form :: forall action. String -> Prop action
```


#### `formAction`

``` purescript
formAction :: forall action. String -> Prop action
```


#### `formEncType`

``` purescript
formEncType :: forall action. String -> Prop action
```


#### `formMethod`

``` purescript
formMethod :: forall action. String -> Prop action
```


#### `formNoValidate`

``` purescript
formNoValidate :: forall action. String -> Prop action
```


#### `formTarget`

``` purescript
formTarget :: forall action. String -> Prop action
```


#### `frameBorder`

``` purescript
frameBorder :: forall action. String -> Prop action
```


#### `height`

``` purescript
height :: forall action. String -> Prop action
```


#### `hidden`

``` purescript
hidden :: forall action. String -> Prop action
```


#### `href`

``` purescript
href :: forall action. String -> Prop action
```


#### `hrefLang`

``` purescript
hrefLang :: forall action. String -> Prop action
```


#### `htmlFor`

``` purescript
htmlFor :: forall action. String -> Prop action
```


#### `httpEquiv`

``` purescript
httpEquiv :: forall action. String -> Prop action
```


#### `icon`

``` purescript
icon :: forall action. String -> Prop action
```


#### `_id`

``` purescript
_id :: forall action. String -> Prop action
```


#### `label`

``` purescript
label :: forall action. String -> Prop action
```


#### `lang`

``` purescript
lang :: forall action. String -> Prop action
```


#### `list`

``` purescript
list :: forall action. String -> Prop action
```


#### `loop`

``` purescript
loop :: forall action. String -> Prop action
```


#### `manifest`

``` purescript
manifest :: forall action. String -> Prop action
```


#### `marginHeight`

``` purescript
marginHeight :: forall action. String -> Prop action
```


#### `marginWidth`

``` purescript
marginWidth :: forall action. String -> Prop action
```


#### `max`

``` purescript
max :: forall action. String -> Prop action
```


#### `maxLength`

``` purescript
maxLength :: forall action. String -> Prop action
```


#### `media`

``` purescript
media :: forall action. String -> Prop action
```


#### `mediaGroup`

``` purescript
mediaGroup :: forall action. String -> Prop action
```


#### `method`

``` purescript
method :: forall action. String -> Prop action
```


#### `min`

``` purescript
min :: forall action. String -> Prop action
```


#### `multiple`

``` purescript
multiple :: forall action. String -> Prop action
```


#### `muted`

``` purescript
muted :: forall action. String -> Prop action
```


#### `name`

``` purescript
name :: forall action. String -> Prop action
```


#### `noValidate`

``` purescript
noValidate :: forall action. String -> Prop action
```


#### `open`

``` purescript
open :: forall action. String -> Prop action
```


#### `pattern`

``` purescript
pattern :: forall action. String -> Prop action
```


#### `placeholder`

``` purescript
placeholder :: forall action. String -> Prop action
```


#### `poster`

``` purescript
poster :: forall action. String -> Prop action
```


#### `preload`

``` purescript
preload :: forall action. String -> Prop action
```


#### `radioGroup`

``` purescript
radioGroup :: forall action. String -> Prop action
```


#### `readOnly`

``` purescript
readOnly :: forall action. String -> Prop action
```


#### `rel`

``` purescript
rel :: forall action. String -> Prop action
```


#### `required`

``` purescript
required :: forall action. String -> Prop action
```


#### `role`

``` purescript
role :: forall action. String -> Prop action
```


#### `rows`

``` purescript
rows :: forall action. String -> Prop action
```


#### `rowSpan`

``` purescript
rowSpan :: forall action. String -> Prop action
```


#### `sandbox`

``` purescript
sandbox :: forall action. String -> Prop action
```


#### `scope`

``` purescript
scope :: forall action. String -> Prop action
```


#### `scrolling`

``` purescript
scrolling :: forall action. String -> Prop action
```


#### `seamless`

``` purescript
seamless :: forall action. String -> Prop action
```


#### `selected`

``` purescript
selected :: forall action. String -> Prop action
```


#### `shape`

``` purescript
shape :: forall action. String -> Prop action
```


#### `size`

``` purescript
size :: forall action. String -> Prop action
```


#### `sizes`

``` purescript
sizes :: forall action. String -> Prop action
```


#### `span`

``` purescript
span :: forall action. String -> Prop action
```


#### `spellCheck`

``` purescript
spellCheck :: forall action. String -> Prop action
```


#### `src`

``` purescript
src :: forall action. String -> Prop action
```


#### `srcDoc`

``` purescript
srcDoc :: forall action. String -> Prop action
```


#### `srcSet`

``` purescript
srcSet :: forall action. String -> Prop action
```


#### `start`

``` purescript
start :: forall action. String -> Prop action
```


#### `step`

``` purescript
step :: forall action. String -> Prop action
```


#### `tabIndex`

``` purescript
tabIndex :: forall action. String -> Prop action
```


#### `target`

``` purescript
target :: forall action. String -> Prop action
```


#### `title`

``` purescript
title :: forall action. String -> Prop action
```


#### `_type`

``` purescript
_type :: forall action. String -> Prop action
```


#### `useMap`

``` purescript
useMap :: forall action. String -> Prop action
```


#### `value`

``` purescript
value :: forall action. String -> Prop action
```


#### `width`

``` purescript
width :: forall action. String -> Prop action
```


#### `wmode`

``` purescript
wmode :: forall action. String -> Prop action
```



## Module Thermite.Html.Elements

#### `a`

``` purescript
a :: forall action. Props action -> [Html action] -> Html action
```


#### `a'`

``` purescript
a' :: forall action. [Html action] -> Html action
```


#### `abbr`

``` purescript
abbr :: forall action. Props action -> [Html action] -> Html action
```


#### `abbr'`

``` purescript
abbr' :: forall action. [Html action] -> Html action
```


#### `address`

``` purescript
address :: forall action. Props action -> [Html action] -> Html action
```


#### `address'`

``` purescript
address' :: forall action. [Html action] -> Html action
```


#### `area`

``` purescript
area :: forall action. Props action -> [Html action] -> Html action
```


#### `area'`

``` purescript
area' :: forall action. [Html action] -> Html action
```


#### `article`

``` purescript
article :: forall action. Props action -> [Html action] -> Html action
```


#### `article'`

``` purescript
article' :: forall action. [Html action] -> Html action
```


#### `aside`

``` purescript
aside :: forall action. Props action -> [Html action] -> Html action
```


#### `aside'`

``` purescript
aside' :: forall action. [Html action] -> Html action
```


#### `audio`

``` purescript
audio :: forall action. Props action -> [Html action] -> Html action
```


#### `audio'`

``` purescript
audio' :: forall action. [Html action] -> Html action
```


#### `b`

``` purescript
b :: forall action. Props action -> [Html action] -> Html action
```


#### `b'`

``` purescript
b' :: forall action. [Html action] -> Html action
```


#### `base`

``` purescript
base :: forall action. Props action -> [Html action] -> Html action
```


#### `base'`

``` purescript
base' :: forall action. [Html action] -> Html action
```


#### `bdi`

``` purescript
bdi :: forall action. Props action -> [Html action] -> Html action
```


#### `bdi'`

``` purescript
bdi' :: forall action. [Html action] -> Html action
```


#### `bdo`

``` purescript
bdo :: forall action. Props action -> [Html action] -> Html action
```


#### `bdo'`

``` purescript
bdo' :: forall action. [Html action] -> Html action
```


#### `big`

``` purescript
big :: forall action. Props action -> [Html action] -> Html action
```


#### `big'`

``` purescript
big' :: forall action. [Html action] -> Html action
```


#### `blockquote`

``` purescript
blockquote :: forall action. Props action -> [Html action] -> Html action
```


#### `blockquote'`

``` purescript
blockquote' :: forall action. [Html action] -> Html action
```


#### `body`

``` purescript
body :: forall action. Props action -> [Html action] -> Html action
```


#### `body'`

``` purescript
body' :: forall action. [Html action] -> Html action
```


#### `br`

``` purescript
br :: forall action. Props action -> [Html action] -> Html action
```


#### `br'`

``` purescript
br' :: forall action. [Html action] -> Html action
```


#### `button`

``` purescript
button :: forall action. Props action -> [Html action] -> Html action
```


#### `button'`

``` purescript
button' :: forall action. [Html action] -> Html action
```


#### `canvas`

``` purescript
canvas :: forall action. Props action -> [Html action] -> Html action
```


#### `canvas'`

``` purescript
canvas' :: forall action. [Html action] -> Html action
```


#### `caption`

``` purescript
caption :: forall action. Props action -> [Html action] -> Html action
```


#### `caption'`

``` purescript
caption' :: forall action. [Html action] -> Html action
```


#### `cite`

``` purescript
cite :: forall action. Props action -> [Html action] -> Html action
```


#### `cite'`

``` purescript
cite' :: forall action. [Html action] -> Html action
```


#### `code`

``` purescript
code :: forall action. Props action -> [Html action] -> Html action
```


#### `code'`

``` purescript
code' :: forall action. [Html action] -> Html action
```


#### `col`

``` purescript
col :: forall action. Props action -> [Html action] -> Html action
```


#### `col'`

``` purescript
col' :: forall action. [Html action] -> Html action
```


#### `colgroup`

``` purescript
colgroup :: forall action. Props action -> [Html action] -> Html action
```


#### `colgroup'`

``` purescript
colgroup' :: forall action. [Html action] -> Html action
```


#### `_data`

``` purescript
_data :: forall action. Props action -> [Html action] -> Html action
```


#### `_data'`

``` purescript
_data' :: forall action. [Html action] -> Html action
```


#### `datalist`

``` purescript
datalist :: forall action. Props action -> [Html action] -> Html action
```


#### `datalist'`

``` purescript
datalist' :: forall action. [Html action] -> Html action
```


#### `dd`

``` purescript
dd :: forall action. Props action -> [Html action] -> Html action
```


#### `dd'`

``` purescript
dd' :: forall action. [Html action] -> Html action
```


#### `del`

``` purescript
del :: forall action. Props action -> [Html action] -> Html action
```


#### `del'`

``` purescript
del' :: forall action. [Html action] -> Html action
```


#### `details`

``` purescript
details :: forall action. Props action -> [Html action] -> Html action
```


#### `details'`

``` purescript
details' :: forall action. [Html action] -> Html action
```


#### `dfn`

``` purescript
dfn :: forall action. Props action -> [Html action] -> Html action
```


#### `dfn'`

``` purescript
dfn' :: forall action. [Html action] -> Html action
```


#### `dialog`

``` purescript
dialog :: forall action. Props action -> [Html action] -> Html action
```


#### `dialog'`

``` purescript
dialog' :: forall action. [Html action] -> Html action
```


#### `div`

``` purescript
div :: forall action. Props action -> [Html action] -> Html action
```


#### `div'`

``` purescript
div' :: forall action. [Html action] -> Html action
```


#### `dl`

``` purescript
dl :: forall action. Props action -> [Html action] -> Html action
```


#### `dl'`

``` purescript
dl' :: forall action. [Html action] -> Html action
```


#### `dt`

``` purescript
dt :: forall action. Props action -> [Html action] -> Html action
```


#### `dt'`

``` purescript
dt' :: forall action. [Html action] -> Html action
```


#### `em`

``` purescript
em :: forall action. Props action -> [Html action] -> Html action
```


#### `em'`

``` purescript
em' :: forall action. [Html action] -> Html action
```


#### `embed`

``` purescript
embed :: forall action. Props action -> [Html action] -> Html action
```


#### `embed'`

``` purescript
embed' :: forall action. [Html action] -> Html action
```


#### `fieldset`

``` purescript
fieldset :: forall action. Props action -> [Html action] -> Html action
```


#### `fieldset'`

``` purescript
fieldset' :: forall action. [Html action] -> Html action
```


#### `figcaption`

``` purescript
figcaption :: forall action. Props action -> [Html action] -> Html action
```


#### `figcaption'`

``` purescript
figcaption' :: forall action. [Html action] -> Html action
```


#### `figure`

``` purescript
figure :: forall action. Props action -> [Html action] -> Html action
```


#### `figure'`

``` purescript
figure' :: forall action. [Html action] -> Html action
```


#### `footer`

``` purescript
footer :: forall action. Props action -> [Html action] -> Html action
```


#### `footer'`

``` purescript
footer' :: forall action. [Html action] -> Html action
```


#### `form`

``` purescript
form :: forall action. Props action -> [Html action] -> Html action
```


#### `form'`

``` purescript
form' :: forall action. [Html action] -> Html action
```


#### `h1`

``` purescript
h1 :: forall action. Props action -> [Html action] -> Html action
```


#### `h1'`

``` purescript
h1' :: forall action. [Html action] -> Html action
```


#### `h2`

``` purescript
h2 :: forall action. Props action -> [Html action] -> Html action
```


#### `h2'`

``` purescript
h2' :: forall action. [Html action] -> Html action
```


#### `h3`

``` purescript
h3 :: forall action. Props action -> [Html action] -> Html action
```


#### `h3'`

``` purescript
h3' :: forall action. [Html action] -> Html action
```


#### `h4`

``` purescript
h4 :: forall action. Props action -> [Html action] -> Html action
```


#### `h4'`

``` purescript
h4' :: forall action. [Html action] -> Html action
```


#### `h5`

``` purescript
h5 :: forall action. Props action -> [Html action] -> Html action
```


#### `h5'`

``` purescript
h5' :: forall action. [Html action] -> Html action
```


#### `h6`

``` purescript
h6 :: forall action. Props action -> [Html action] -> Html action
```


#### `h6'`

``` purescript
h6' :: forall action. [Html action] -> Html action
```


#### `head`

``` purescript
head :: forall action. Props action -> [Html action] -> Html action
```


#### `head'`

``` purescript
head' :: forall action. [Html action] -> Html action
```


#### `header`

``` purescript
header :: forall action. Props action -> [Html action] -> Html action
```


#### `header'`

``` purescript
header' :: forall action. [Html action] -> Html action
```


#### `hr`

``` purescript
hr :: forall action. Props action -> [Html action] -> Html action
```


#### `hr'`

``` purescript
hr' :: forall action. [Html action] -> Html action
```


#### `html`

``` purescript
html :: forall action. Props action -> [Html action] -> Html action
```


#### `html'`

``` purescript
html' :: forall action. [Html action] -> Html action
```


#### `i`

``` purescript
i :: forall action. Props action -> [Html action] -> Html action
```


#### `i'`

``` purescript
i' :: forall action. [Html action] -> Html action
```


#### `iframe`

``` purescript
iframe :: forall action. Props action -> [Html action] -> Html action
```


#### `iframe'`

``` purescript
iframe' :: forall action. [Html action] -> Html action
```


#### `img`

``` purescript
img :: forall action. Props action -> [Html action] -> Html action
```


#### `img'`

``` purescript
img' :: forall action. [Html action] -> Html action
```


#### `input`

``` purescript
input :: forall action. Props action -> [Html action] -> Html action
```


#### `input'`

``` purescript
input' :: forall action. [Html action] -> Html action
```


#### `ins`

``` purescript
ins :: forall action. Props action -> [Html action] -> Html action
```


#### `ins'`

``` purescript
ins' :: forall action. [Html action] -> Html action
```


#### `kbd`

``` purescript
kbd :: forall action. Props action -> [Html action] -> Html action
```


#### `kbd'`

``` purescript
kbd' :: forall action. [Html action] -> Html action
```


#### `keygen`

``` purescript
keygen :: forall action. Props action -> [Html action] -> Html action
```


#### `keygen'`

``` purescript
keygen' :: forall action. [Html action] -> Html action
```


#### `label`

``` purescript
label :: forall action. Props action -> [Html action] -> Html action
```


#### `label'`

``` purescript
label' :: forall action. [Html action] -> Html action
```


#### `legend`

``` purescript
legend :: forall action. Props action -> [Html action] -> Html action
```


#### `legend'`

``` purescript
legend' :: forall action. [Html action] -> Html action
```


#### `li`

``` purescript
li :: forall action. Props action -> [Html action] -> Html action
```


#### `li'`

``` purescript
li' :: forall action. [Html action] -> Html action
```


#### `link`

``` purescript
link :: forall action. Props action -> [Html action] -> Html action
```


#### `link'`

``` purescript
link' :: forall action. [Html action] -> Html action
```


#### `main`

``` purescript
main :: forall action. Props action -> [Html action] -> Html action
```


#### `main'`

``` purescript
main' :: forall action. [Html action] -> Html action
```


#### `map`

``` purescript
map :: forall action. Props action -> [Html action] -> Html action
```


#### `map'`

``` purescript
map' :: forall action. [Html action] -> Html action
```


#### `mark`

``` purescript
mark :: forall action. Props action -> [Html action] -> Html action
```


#### `mark'`

``` purescript
mark' :: forall action. [Html action] -> Html action
```


#### `menu`

``` purescript
menu :: forall action. Props action -> [Html action] -> Html action
```


#### `menu'`

``` purescript
menu' :: forall action. [Html action] -> Html action
```


#### `menuitem`

``` purescript
menuitem :: forall action. Props action -> [Html action] -> Html action
```


#### `menuitem'`

``` purescript
menuitem' :: forall action. [Html action] -> Html action
```


#### `meta`

``` purescript
meta :: forall action. Props action -> [Html action] -> Html action
```


#### `meta'`

``` purescript
meta' :: forall action. [Html action] -> Html action
```


#### `meter`

``` purescript
meter :: forall action. Props action -> [Html action] -> Html action
```


#### `meter'`

``` purescript
meter' :: forall action. [Html action] -> Html action
```


#### `nav`

``` purescript
nav :: forall action. Props action -> [Html action] -> Html action
```


#### `nav'`

``` purescript
nav' :: forall action. [Html action] -> Html action
```


#### `noscript`

``` purescript
noscript :: forall action. Props action -> [Html action] -> Html action
```


#### `noscript'`

``` purescript
noscript' :: forall action. [Html action] -> Html action
```


#### `object`

``` purescript
object :: forall action. Props action -> [Html action] -> Html action
```


#### `object'`

``` purescript
object' :: forall action. [Html action] -> Html action
```


#### `ol`

``` purescript
ol :: forall action. Props action -> [Html action] -> Html action
```


#### `ol'`

``` purescript
ol' :: forall action. [Html action] -> Html action
```


#### `optgroup`

``` purescript
optgroup :: forall action. Props action -> [Html action] -> Html action
```


#### `optgroup'`

``` purescript
optgroup' :: forall action. [Html action] -> Html action
```


#### `option`

``` purescript
option :: forall action. Props action -> [Html action] -> Html action
```


#### `option'`

``` purescript
option' :: forall action. [Html action] -> Html action
```


#### `output`

``` purescript
output :: forall action. Props action -> [Html action] -> Html action
```


#### `output'`

``` purescript
output' :: forall action. [Html action] -> Html action
```


#### `p`

``` purescript
p :: forall action. Props action -> [Html action] -> Html action
```


#### `p'`

``` purescript
p' :: forall action. [Html action] -> Html action
```


#### `param`

``` purescript
param :: forall action. Props action -> [Html action] -> Html action
```


#### `param'`

``` purescript
param' :: forall action. [Html action] -> Html action
```


#### `picture`

``` purescript
picture :: forall action. Props action -> [Html action] -> Html action
```


#### `picture'`

``` purescript
picture' :: forall action. [Html action] -> Html action
```


#### `pre`

``` purescript
pre :: forall action. Props action -> [Html action] -> Html action
```


#### `pre'`

``` purescript
pre' :: forall action. [Html action] -> Html action
```


#### `progress`

``` purescript
progress :: forall action. Props action -> [Html action] -> Html action
```


#### `progress'`

``` purescript
progress' :: forall action. [Html action] -> Html action
```


#### `q`

``` purescript
q :: forall action. Props action -> [Html action] -> Html action
```


#### `q'`

``` purescript
q' :: forall action. [Html action] -> Html action
```


#### `rp`

``` purescript
rp :: forall action. Props action -> [Html action] -> Html action
```


#### `rp'`

``` purescript
rp' :: forall action. [Html action] -> Html action
```


#### `rt`

``` purescript
rt :: forall action. Props action -> [Html action] -> Html action
```


#### `rt'`

``` purescript
rt' :: forall action. [Html action] -> Html action
```


#### `ruby`

``` purescript
ruby :: forall action. Props action -> [Html action] -> Html action
```


#### `ruby'`

``` purescript
ruby' :: forall action. [Html action] -> Html action
```


#### `s`

``` purescript
s :: forall action. Props action -> [Html action] -> Html action
```


#### `s'`

``` purescript
s' :: forall action. [Html action] -> Html action
```


#### `samp`

``` purescript
samp :: forall action. Props action -> [Html action] -> Html action
```


#### `samp'`

``` purescript
samp' :: forall action. [Html action] -> Html action
```


#### `script`

``` purescript
script :: forall action. Props action -> [Html action] -> Html action
```


#### `script'`

``` purescript
script' :: forall action. [Html action] -> Html action
```


#### `section`

``` purescript
section :: forall action. Props action -> [Html action] -> Html action
```


#### `section'`

``` purescript
section' :: forall action. [Html action] -> Html action
```


#### `select`

``` purescript
select :: forall action. Props action -> [Html action] -> Html action
```


#### `select'`

``` purescript
select' :: forall action. [Html action] -> Html action
```


#### `small`

``` purescript
small :: forall action. Props action -> [Html action] -> Html action
```


#### `small'`

``` purescript
small' :: forall action. [Html action] -> Html action
```


#### `source`

``` purescript
source :: forall action. Props action -> [Html action] -> Html action
```


#### `source'`

``` purescript
source' :: forall action. [Html action] -> Html action
```


#### `span`

``` purescript
span :: forall action. Props action -> [Html action] -> Html action
```


#### `span'`

``` purescript
span' :: forall action. [Html action] -> Html action
```


#### `strong`

``` purescript
strong :: forall action. Props action -> [Html action] -> Html action
```


#### `strong'`

``` purescript
strong' :: forall action. [Html action] -> Html action
```


#### `style`

``` purescript
style :: forall action. Props action -> [Html action] -> Html action
```


#### `style'`

``` purescript
style' :: forall action. [Html action] -> Html action
```


#### `sub`

``` purescript
sub :: forall action. Props action -> [Html action] -> Html action
```


#### `sub'`

``` purescript
sub' :: forall action. [Html action] -> Html action
```


#### `summary`

``` purescript
summary :: forall action. Props action -> [Html action] -> Html action
```


#### `summary'`

``` purescript
summary' :: forall action. [Html action] -> Html action
```


#### `sup`

``` purescript
sup :: forall action. Props action -> [Html action] -> Html action
```


#### `sup'`

``` purescript
sup' :: forall action. [Html action] -> Html action
```


#### `table`

``` purescript
table :: forall action. Props action -> [Html action] -> Html action
```


#### `table'`

``` purescript
table' :: forall action. [Html action] -> Html action
```


#### `tbody`

``` purescript
tbody :: forall action. Props action -> [Html action] -> Html action
```


#### `tbody'`

``` purescript
tbody' :: forall action. [Html action] -> Html action
```


#### `td`

``` purescript
td :: forall action. Props action -> [Html action] -> Html action
```


#### `td'`

``` purescript
td' :: forall action. [Html action] -> Html action
```


#### `textarea`

``` purescript
textarea :: forall action. Props action -> [Html action] -> Html action
```


#### `textarea'`

``` purescript
textarea' :: forall action. [Html action] -> Html action
```


#### `tfoot`

``` purescript
tfoot :: forall action. Props action -> [Html action] -> Html action
```


#### `tfoot'`

``` purescript
tfoot' :: forall action. [Html action] -> Html action
```


#### `th`

``` purescript
th :: forall action. Props action -> [Html action] -> Html action
```


#### `th'`

``` purescript
th' :: forall action. [Html action] -> Html action
```


#### `thead`

``` purescript
thead :: forall action. Props action -> [Html action] -> Html action
```


#### `thead'`

``` purescript
thead' :: forall action. [Html action] -> Html action
```


#### `time`

``` purescript
time :: forall action. Props action -> [Html action] -> Html action
```


#### `time'`

``` purescript
time' :: forall action. [Html action] -> Html action
```


#### `title`

``` purescript
title :: forall action. Props action -> [Html action] -> Html action
```


#### `title'`

``` purescript
title' :: forall action. [Html action] -> Html action
```


#### `tr`

``` purescript
tr :: forall action. Props action -> [Html action] -> Html action
```


#### `tr'`

``` purescript
tr' :: forall action. [Html action] -> Html action
```


#### `track`

``` purescript
track :: forall action. Props action -> [Html action] -> Html action
```


#### `track'`

``` purescript
track' :: forall action. [Html action] -> Html action
```


#### `u`

``` purescript
u :: forall action. Props action -> [Html action] -> Html action
```


#### `u'`

``` purescript
u' :: forall action. [Html action] -> Html action
```


#### `ul`

``` purescript
ul :: forall action. Props action -> [Html action] -> Html action
```


#### `ul'`

``` purescript
ul' :: forall action. [Html action] -> Html action
```


#### `var`

``` purescript
var :: forall action. Props action -> [Html action] -> Html action
```


#### `var'`

``` purescript
var' :: forall action. [Html action] -> Html action
```


#### `video`

``` purescript
video :: forall action. Props action -> [Html action] -> Html action
```


#### `video'`

``` purescript
video' :: forall action. [Html action] -> Html action
```


#### `wbr`

``` purescript
wbr :: forall action. Props action -> [Html action] -> Html action
```


#### `wbr'`

``` purescript
wbr' :: forall action. [Html action] -> Html action
```



## Module Thermite.Internal

#### `getStateImpl`

``` purescript
getStateImpl :: forall eff state props action. Context state props action -> Eff eff state
```


#### `setStateImpl`

``` purescript
setStateImpl :: forall eff state props action. Context state props action -> state -> Eff eff Unit
```


#### `textImpl`

``` purescript
textImpl :: forall action. String -> Html action
```


#### `createElementImpl`

``` purescript
createElementImpl :: forall action. String -> Props action -> [Html action] -> Html action
```


#### `unsafeAttribute`

``` purescript
unsafeAttribute :: forall action attr. String -> attr -> Prop action
```


#### `event`

``` purescript
event :: forall state props action event. String -> Context state props action -> (event -> action) -> Prop action
```


#### `createClassImpl`

``` purescript
createClassImpl :: forall eff m state props action. (Context state props action -> m Unit -> Eff eff Unit) -> (forall a r. r -> (a -> r) -> Maybe a -> r) -> Spec m state props action -> ComponentClass props eff
```


#### `documentBody`

``` purescript
documentBody :: forall props eff. Eff (dom :: DOM | eff) Node
```


#### `renderToImpl`

``` purescript
renderToImpl :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
```



## Module Thermite.Types

#### `Context`

``` purescript
data Context state props action
```


#### `ComponentClass`

``` purescript
data ComponentClass props (eff :: # !)
```


#### `Prop`

``` purescript
data Prop action
```


#### `Props`

``` purescript
type Props action = [Prop action]
```


#### `Html`

``` purescript
data Html action
```


#### `PerformAction`

``` purescript
type PerformAction props action m = props -> action -> m Unit
```


#### `Render`

``` purescript
type Render state props action = Context state props action -> state -> props -> Html action
```


#### `Spec`

``` purescript
newtype Spec m state props action
  = Spec (SpecRecord m state props action)
```


#### `SpecRecord`

``` purescript
type SpecRecord m state props action = { displayName :: Maybe String, componentWillMount :: Maybe action, render :: Render state props action, performAction :: PerformAction props action m, initialState :: state }
```




