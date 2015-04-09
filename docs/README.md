# Module Documentation

## Module Thermite


This module defines functions for working with React components at a high level:

- `createClass` and `simpleSpec`, which can be used to create a component class
- `render` and `renderTo`, which can be used to render a component class

#### `simpleSpec`

``` purescript
simpleSpec :: forall m state props action. state -> PerformAction props action m -> Render state props action -> Spec m state props action
```

Create a minimal `Spec`. The arguments are, in order:

- The initial component state
- A function for performing actions
- A function for rendering the current state as a HTML document

A `Spec` created using this function can be extended with optional properties using other functions
in this module.

#### `componentWillMount`

``` purescript
componentWillMount :: forall m state props action. action -> Spec m state props action -> Spec m state props action
```

Extend a `Spec` with an action to run when the component will be mounted.

#### `displayName`

``` purescript
displayName :: forall m state props action. String -> Spec m state props action -> Spec m state props action
```

Extend a `Spec` with a display name.

#### `createClass`

``` purescript
createClass :: forall eff state props action. Spec (Action eff state) state props action -> ComponentClass props eff
```

Create a component class from a `Spec`.

#### `render`

``` purescript
render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
```

Render a component class to the document body.

#### `renderTo`

``` purescript
renderTo :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
```

Render a component class to the specified node.


## Module Thermite.Types


This module defines types used by the Thermite library.

#### `Context`

``` purescript
data Context state props action
```

The `Context` type represents React's `this` reference.

It is passed to event handlers to allow us to get and set component state.

#### `ComponentClass`

``` purescript
data ComponentClass props (eff :: # !)
```

A component class, the result of React's `createClass` method.

The type parameters capture the properties required by the class, and the effects
its action handlers can have.

#### `Attr`

``` purescript
data Attr action
```

The type of HTML attributes, parameterised by the type of actions they can emit from 
event handlers.

#### `Html`

``` purescript
data Html action
```

The type of HTML elements.

#### `PerformAction`

``` purescript
type PerformAction props action m = props -> action -> m Unit
```

A type synonym for action handlers, which take an action and the current properties
for the component, and return a computation in some monad `m`.

In practice, `m` will be the `Action` monad.

#### `Render`

``` purescript
type Render state props action = Context state props action -> state -> props -> Html action
```

A rendering function, which takes a `Context`, the current state and properties, and 
returns a HTML document.

#### `Spec`

``` purescript
newtype Spec m state props action
  = Spec (SpecRecord m state props action)
```

A component specification, which can be passed to `createClass`.

A minimal `Spec` can be built using `simpleSpec`, and extended with optional arguments
using functions in the `Thermite` module.

#### `SpecRecord`

``` purescript
type SpecRecord m state props action = { displayName :: Maybe String, componentWillMount :: Maybe action, render :: Render state props action, performAction :: PerformAction props action m, initialState :: state }
```

A type synonym for the record type which captures the functions which make up a `Spec`.

#### `semigroupAttr`

``` purescript
instance semigroupAttr :: Semigroup (Attr action)
```


#### `monoidAttr`

``` purescript
instance monoidAttr :: Monoid (Attr action)
```



## Module Thermite.Action


This module defines the `Action` monad, which can be used to access the 
component state, and invoke asynchronous actions in the `Eff` monad.

#### `functorActionF`

``` purescript
instance functorActionF :: Functor (ActionF eff state)
```


#### `Action`

``` purescript
data Action eff state a
```

The `Action` monad, parameterized by 

- The row of effects which are allowed in its asynchronous actions
- The component state type
- The return type

#### `runAction`

``` purescript
runAction :: forall eff state props action a. Context state props action -> Action eff state a -> Eff eff Unit
```

Run a computation in the `Action` monad.

#### `getState`

``` purescript
getState :: forall eff state. Action eff state state
```

Get the current component state.

#### `setState`

``` purescript
setState :: forall eff state. state -> Action eff state Unit
```

Update the component state.

#### `modifyState`

``` purescript
modifyState :: forall eff state. (state -> state) -> Action eff state Unit
```

Modify the component state by applying a function.

#### `async`

``` purescript
async :: forall eff state a. ((a -> Eff eff Unit) -> Eff eff Unit) -> Action eff state a
```

Run an asynchronous computation.

The first argument is a function which takes a callback, and starts some asynchronous computation,
invoking the callback when the result is available.

#### `sync`

``` purescript
sync :: forall eff state a. Eff eff a -> Action eff state a
```

Run a synchronous computation.

#### `asyncSetState`

``` purescript
asyncSetState :: forall eff state. ((state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state Unit
```

Set the component state based on the result of an asynchronous computation.

The first argument is a function which takes a callback, and starts some asynchronous computation,
invoking the callback when the new state is available.

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



## Module Thermite.Html


This module defines functions for creating simple HTML documents.

#### `text`

``` purescript
text :: forall action. String -> Html action
```

Create a text node.

#### `createElement`

``` purescript
createElement :: forall action. String -> Attr action -> [Html action] -> Html action
```

Create a HTML element from a tag name, a set of attributes and a collection of child nodes.


## Module Thermite.Html.Attributes


This module defines helper functions for creating HTML attributes.

#### `accept`

``` purescript
accept :: forall action. String -> Attr action
```


#### `acceptCharset`

``` purescript
acceptCharset :: forall action. String -> Attr action
```


#### `accessKey`

``` purescript
accessKey :: forall action. String -> Attr action
```


#### `action`

``` purescript
action :: forall action. String -> Attr action
```


#### `allowFullScreen`

``` purescript
allowFullScreen :: forall action. String -> Attr action
```


#### `allowTransparency`

``` purescript
allowTransparency :: forall action. String -> Attr action
```


#### `alt`

``` purescript
alt :: forall action. String -> Attr action
```


#### `async`

``` purescript
async :: forall action. String -> Attr action
```


#### `autoComplete`

``` purescript
autoComplete :: forall action. String -> Attr action
```


#### `autoFocus`

``` purescript
autoFocus :: forall action. Boolean -> Attr action
```


#### `autoPlay`

``` purescript
autoPlay :: forall action. String -> Attr action
```


#### `cellPadding`

``` purescript
cellPadding :: forall action. String -> Attr action
```


#### `cellSpacing`

``` purescript
cellSpacing :: forall action. String -> Attr action
```


#### `charSet`

``` purescript
charSet :: forall action. String -> Attr action
```


#### `checked`

``` purescript
checked :: forall action. String -> Attr action
```


#### `classID`

``` purescript
classID :: forall action. String -> Attr action
```


#### `className`

``` purescript
className :: forall action. String -> Attr action
```


#### `cols`

``` purescript
cols :: forall action. String -> Attr action
```


#### `colSpan`

``` purescript
colSpan :: forall action. String -> Attr action
```


#### `content`

``` purescript
content :: forall action. String -> Attr action
```


#### `contentEditable`

``` purescript
contentEditable :: forall action. String -> Attr action
```


#### `contextMenu`

``` purescript
contextMenu :: forall action. String -> Attr action
```


#### `controls`

``` purescript
controls :: forall action. String -> Attr action
```


#### `coords`

``` purescript
coords :: forall action. String -> Attr action
```


#### `crossOrigin`

``` purescript
crossOrigin :: forall action. String -> Attr action
```


#### `dateTime`

``` purescript
dateTime :: forall action. String -> Attr action
```


#### `defer`

``` purescript
defer :: forall action. String -> Attr action
```


#### `dir`

``` purescript
dir :: forall action. String -> Attr action
```


#### `disabled`

``` purescript
disabled :: forall action. Boolean -> Attr action
```


#### `download`

``` purescript
download :: forall action. String -> Attr action
```


#### `draggable`

``` purescript
draggable :: forall action. String -> Attr action
```


#### `encType`

``` purescript
encType :: forall action. String -> Attr action
```


#### `form`

``` purescript
form :: forall action. String -> Attr action
```


#### `formAction`

``` purescript
formAction :: forall action. String -> Attr action
```


#### `formEncType`

``` purescript
formEncType :: forall action. String -> Attr action
```


#### `formMethod`

``` purescript
formMethod :: forall action. String -> Attr action
```


#### `formNoValidate`

``` purescript
formNoValidate :: forall action. String -> Attr action
```


#### `formTarget`

``` purescript
formTarget :: forall action. String -> Attr action
```


#### `frameBorder`

``` purescript
frameBorder :: forall action. String -> Attr action
```


#### `height`

``` purescript
height :: forall action. String -> Attr action
```


#### `hidden`

``` purescript
hidden :: forall action. String -> Attr action
```


#### `href`

``` purescript
href :: forall action. String -> Attr action
```


#### `hrefLang`

``` purescript
hrefLang :: forall action. String -> Attr action
```


#### `htmlFor`

``` purescript
htmlFor :: forall action. String -> Attr action
```


#### `httpEquiv`

``` purescript
httpEquiv :: forall action. String -> Attr action
```


#### `icon`

``` purescript
icon :: forall action. String -> Attr action
```


#### `_id`

``` purescript
_id :: forall action. String -> Attr action
```


#### `key`

``` purescript
key :: forall action. String -> Attr action
```


#### `label`

``` purescript
label :: forall action. String -> Attr action
```


#### `lang`

``` purescript
lang :: forall action. String -> Attr action
```


#### `list`

``` purescript
list :: forall action. String -> Attr action
```


#### `loop`

``` purescript
loop :: forall action. String -> Attr action
```


#### `manifest`

``` purescript
manifest :: forall action. String -> Attr action
```


#### `marginHeight`

``` purescript
marginHeight :: forall action. String -> Attr action
```


#### `marginWidth`

``` purescript
marginWidth :: forall action. String -> Attr action
```


#### `max`

``` purescript
max :: forall action. String -> Attr action
```


#### `maxLength`

``` purescript
maxLength :: forall action. String -> Attr action
```


#### `media`

``` purescript
media :: forall action. String -> Attr action
```


#### `mediaGroup`

``` purescript
mediaGroup :: forall action. String -> Attr action
```


#### `method`

``` purescript
method :: forall action. String -> Attr action
```


#### `min`

``` purescript
min :: forall action. String -> Attr action
```


#### `multiple`

``` purescript
multiple :: forall action. String -> Attr action
```


#### `muted`

``` purescript
muted :: forall action. String -> Attr action
```


#### `name`

``` purescript
name :: forall action. String -> Attr action
```


#### `noValidate`

``` purescript
noValidate :: forall action. String -> Attr action
```


#### `open`

``` purescript
open :: forall action. String -> Attr action
```


#### `pattern`

``` purescript
pattern :: forall action. String -> Attr action
```


#### `placeholder`

``` purescript
placeholder :: forall action. String -> Attr action
```


#### `poster`

``` purescript
poster :: forall action. String -> Attr action
```


#### `preload`

``` purescript
preload :: forall action. String -> Attr action
```


#### `radioGroup`

``` purescript
radioGroup :: forall action. String -> Attr action
```


#### `readOnly`

``` purescript
readOnly :: forall action. String -> Attr action
```


#### `rel`

``` purescript
rel :: forall action. String -> Attr action
```


#### `required`

``` purescript
required :: forall action. String -> Attr action
```


#### `role`

``` purescript
role :: forall action. String -> Attr action
```


#### `rows`

``` purescript
rows :: forall action. String -> Attr action
```


#### `rowSpan`

``` purescript
rowSpan :: forall action. String -> Attr action
```


#### `sandbox`

``` purescript
sandbox :: forall action. String -> Attr action
```


#### `scope`

``` purescript
scope :: forall action. String -> Attr action
```


#### `scrolling`

``` purescript
scrolling :: forall action. String -> Attr action
```


#### `seamless`

``` purescript
seamless :: forall action. String -> Attr action
```


#### `selected`

``` purescript
selected :: forall action. String -> Attr action
```


#### `shape`

``` purescript
shape :: forall action. String -> Attr action
```


#### `size`

``` purescript
size :: forall action. String -> Attr action
```


#### `sizes`

``` purescript
sizes :: forall action. String -> Attr action
```


#### `span`

``` purescript
span :: forall action. String -> Attr action
```


#### `spellCheck`

``` purescript
spellCheck :: forall action. String -> Attr action
```


#### `src`

``` purescript
src :: forall action. String -> Attr action
```


#### `srcDoc`

``` purescript
srcDoc :: forall action. String -> Attr action
```


#### `srcSet`

``` purescript
srcSet :: forall action. String -> Attr action
```


#### `start`

``` purescript
start :: forall action. String -> Attr action
```


#### `step`

``` purescript
step :: forall action. String -> Attr action
```


#### `tabIndex`

``` purescript
tabIndex :: forall action. String -> Attr action
```


#### `target`

``` purescript
target :: forall action. String -> Attr action
```


#### `title`

``` purescript
title :: forall action. String -> Attr action
```


#### `_type`

``` purescript
_type :: forall action. String -> Attr action
```


#### `useMap`

``` purescript
useMap :: forall action. String -> Attr action
```


#### `value`

``` purescript
value :: forall action. String -> Attr action
```


#### `width`

``` purescript
width :: forall action. String -> Attr action
```


#### `wmode`

``` purescript
wmode :: forall action. String -> Attr action
```



## Module Thermite.Html.Elements


This module defines helper functions for creating HTML elements.

#### `a`

``` purescript
a :: forall action. Attr action -> [Html action] -> Html action
```


#### `a'`

``` purescript
a' :: forall action. [Html action] -> Html action
```


#### `abbr`

``` purescript
abbr :: forall action. Attr action -> [Html action] -> Html action
```


#### `abbr'`

``` purescript
abbr' :: forall action. [Html action] -> Html action
```


#### `address`

``` purescript
address :: forall action. Attr action -> [Html action] -> Html action
```


#### `address'`

``` purescript
address' :: forall action. [Html action] -> Html action
```


#### `area`

``` purescript
area :: forall action. Attr action -> [Html action] -> Html action
```


#### `area'`

``` purescript
area' :: forall action. [Html action] -> Html action
```


#### `article`

``` purescript
article :: forall action. Attr action -> [Html action] -> Html action
```


#### `article'`

``` purescript
article' :: forall action. [Html action] -> Html action
```


#### `aside`

``` purescript
aside :: forall action. Attr action -> [Html action] -> Html action
```


#### `aside'`

``` purescript
aside' :: forall action. [Html action] -> Html action
```


#### `audio`

``` purescript
audio :: forall action. Attr action -> [Html action] -> Html action
```


#### `audio'`

``` purescript
audio' :: forall action. [Html action] -> Html action
```


#### `b`

``` purescript
b :: forall action. Attr action -> [Html action] -> Html action
```


#### `b'`

``` purescript
b' :: forall action. [Html action] -> Html action
```


#### `base`

``` purescript
base :: forall action. Attr action -> [Html action] -> Html action
```


#### `base'`

``` purescript
base' :: forall action. [Html action] -> Html action
```


#### `bdi`

``` purescript
bdi :: forall action. Attr action -> [Html action] -> Html action
```


#### `bdi'`

``` purescript
bdi' :: forall action. [Html action] -> Html action
```


#### `bdo`

``` purescript
bdo :: forall action. Attr action -> [Html action] -> Html action
```


#### `bdo'`

``` purescript
bdo' :: forall action. [Html action] -> Html action
```


#### `big`

``` purescript
big :: forall action. Attr action -> [Html action] -> Html action
```


#### `big'`

``` purescript
big' :: forall action. [Html action] -> Html action
```


#### `blockquote`

``` purescript
blockquote :: forall action. Attr action -> [Html action] -> Html action
```


#### `blockquote'`

``` purescript
blockquote' :: forall action. [Html action] -> Html action
```


#### `body`

``` purescript
body :: forall action. Attr action -> [Html action] -> Html action
```


#### `body'`

``` purescript
body' :: forall action. [Html action] -> Html action
```


#### `br`

``` purescript
br :: forall action. Attr action -> [Html action] -> Html action
```


#### `br'`

``` purescript
br' :: forall action. [Html action] -> Html action
```


#### `button`

``` purescript
button :: forall action. Attr action -> [Html action] -> Html action
```


#### `button'`

``` purescript
button' :: forall action. [Html action] -> Html action
```


#### `canvas`

``` purescript
canvas :: forall action. Attr action -> [Html action] -> Html action
```


#### `canvas'`

``` purescript
canvas' :: forall action. [Html action] -> Html action
```


#### `caption`

``` purescript
caption :: forall action. Attr action -> [Html action] -> Html action
```


#### `caption'`

``` purescript
caption' :: forall action. [Html action] -> Html action
```


#### `cite`

``` purescript
cite :: forall action. Attr action -> [Html action] -> Html action
```


#### `cite'`

``` purescript
cite' :: forall action. [Html action] -> Html action
```


#### `code`

``` purescript
code :: forall action. Attr action -> [Html action] -> Html action
```


#### `code'`

``` purescript
code' :: forall action. [Html action] -> Html action
```


#### `col`

``` purescript
col :: forall action. Attr action -> [Html action] -> Html action
```


#### `col'`

``` purescript
col' :: forall action. [Html action] -> Html action
```


#### `colgroup`

``` purescript
colgroup :: forall action. Attr action -> [Html action] -> Html action
```


#### `colgroup'`

``` purescript
colgroup' :: forall action. [Html action] -> Html action
```


#### `_data`

``` purescript
_data :: forall action. Attr action -> [Html action] -> Html action
```


#### `_data'`

``` purescript
_data' :: forall action. [Html action] -> Html action
```


#### `datalist`

``` purescript
datalist :: forall action. Attr action -> [Html action] -> Html action
```


#### `datalist'`

``` purescript
datalist' :: forall action. [Html action] -> Html action
```


#### `dd`

``` purescript
dd :: forall action. Attr action -> [Html action] -> Html action
```


#### `dd'`

``` purescript
dd' :: forall action. [Html action] -> Html action
```


#### `del`

``` purescript
del :: forall action. Attr action -> [Html action] -> Html action
```


#### `del'`

``` purescript
del' :: forall action. [Html action] -> Html action
```


#### `details`

``` purescript
details :: forall action. Attr action -> [Html action] -> Html action
```


#### `details'`

``` purescript
details' :: forall action. [Html action] -> Html action
```


#### `dfn`

``` purescript
dfn :: forall action. Attr action -> [Html action] -> Html action
```


#### `dfn'`

``` purescript
dfn' :: forall action. [Html action] -> Html action
```


#### `dialog`

``` purescript
dialog :: forall action. Attr action -> [Html action] -> Html action
```


#### `dialog'`

``` purescript
dialog' :: forall action. [Html action] -> Html action
```


#### `div`

``` purescript
div :: forall action. Attr action -> [Html action] -> Html action
```


#### `div'`

``` purescript
div' :: forall action. [Html action] -> Html action
```


#### `dl`

``` purescript
dl :: forall action. Attr action -> [Html action] -> Html action
```


#### `dl'`

``` purescript
dl' :: forall action. [Html action] -> Html action
```


#### `dt`

``` purescript
dt :: forall action. Attr action -> [Html action] -> Html action
```


#### `dt'`

``` purescript
dt' :: forall action. [Html action] -> Html action
```


#### `em`

``` purescript
em :: forall action. Attr action -> [Html action] -> Html action
```


#### `em'`

``` purescript
em' :: forall action. [Html action] -> Html action
```


#### `embed`

``` purescript
embed :: forall action. Attr action -> [Html action] -> Html action
```


#### `embed'`

``` purescript
embed' :: forall action. [Html action] -> Html action
```


#### `fieldset`

``` purescript
fieldset :: forall action. Attr action -> [Html action] -> Html action
```


#### `fieldset'`

``` purescript
fieldset' :: forall action. [Html action] -> Html action
```


#### `figcaption`

``` purescript
figcaption :: forall action. Attr action -> [Html action] -> Html action
```


#### `figcaption'`

``` purescript
figcaption' :: forall action. [Html action] -> Html action
```


#### `figure`

``` purescript
figure :: forall action. Attr action -> [Html action] -> Html action
```


#### `figure'`

``` purescript
figure' :: forall action. [Html action] -> Html action
```


#### `footer`

``` purescript
footer :: forall action. Attr action -> [Html action] -> Html action
```


#### `footer'`

``` purescript
footer' :: forall action. [Html action] -> Html action
```


#### `form`

``` purescript
form :: forall action. Attr action -> [Html action] -> Html action
```


#### `form'`

``` purescript
form' :: forall action. [Html action] -> Html action
```


#### `h1`

``` purescript
h1 :: forall action. Attr action -> [Html action] -> Html action
```


#### `h1'`

``` purescript
h1' :: forall action. [Html action] -> Html action
```


#### `h2`

``` purescript
h2 :: forall action. Attr action -> [Html action] -> Html action
```


#### `h2'`

``` purescript
h2' :: forall action. [Html action] -> Html action
```


#### `h3`

``` purescript
h3 :: forall action. Attr action -> [Html action] -> Html action
```


#### `h3'`

``` purescript
h3' :: forall action. [Html action] -> Html action
```


#### `h4`

``` purescript
h4 :: forall action. Attr action -> [Html action] -> Html action
```


#### `h4'`

``` purescript
h4' :: forall action. [Html action] -> Html action
```


#### `h5`

``` purescript
h5 :: forall action. Attr action -> [Html action] -> Html action
```


#### `h5'`

``` purescript
h5' :: forall action. [Html action] -> Html action
```


#### `h6`

``` purescript
h6 :: forall action. Attr action -> [Html action] -> Html action
```


#### `h6'`

``` purescript
h6' :: forall action. [Html action] -> Html action
```


#### `head`

``` purescript
head :: forall action. Attr action -> [Html action] -> Html action
```


#### `head'`

``` purescript
head' :: forall action. [Html action] -> Html action
```


#### `header`

``` purescript
header :: forall action. Attr action -> [Html action] -> Html action
```


#### `header'`

``` purescript
header' :: forall action. [Html action] -> Html action
```


#### `hr`

``` purescript
hr :: forall action. Attr action -> [Html action] -> Html action
```


#### `hr'`

``` purescript
hr' :: forall action. [Html action] -> Html action
```


#### `html`

``` purescript
html :: forall action. Attr action -> [Html action] -> Html action
```


#### `html'`

``` purescript
html' :: forall action. [Html action] -> Html action
```


#### `i`

``` purescript
i :: forall action. Attr action -> [Html action] -> Html action
```


#### `i'`

``` purescript
i' :: forall action. [Html action] -> Html action
```


#### `iframe`

``` purescript
iframe :: forall action. Attr action -> [Html action] -> Html action
```


#### `iframe'`

``` purescript
iframe' :: forall action. [Html action] -> Html action
```


#### `img`

``` purescript
img :: forall action. Attr action -> [Html action] -> Html action
```


#### `img'`

``` purescript
img' :: forall action. [Html action] -> Html action
```


#### `input`

``` purescript
input :: forall action. Attr action -> [Html action] -> Html action
```


#### `input'`

``` purescript
input' :: forall action. [Html action] -> Html action
```


#### `ins`

``` purescript
ins :: forall action. Attr action -> [Html action] -> Html action
```


#### `ins'`

``` purescript
ins' :: forall action. [Html action] -> Html action
```


#### `kbd`

``` purescript
kbd :: forall action. Attr action -> [Html action] -> Html action
```


#### `kbd'`

``` purescript
kbd' :: forall action. [Html action] -> Html action
```


#### `keygen`

``` purescript
keygen :: forall action. Attr action -> [Html action] -> Html action
```


#### `keygen'`

``` purescript
keygen' :: forall action. [Html action] -> Html action
```


#### `label`

``` purescript
label :: forall action. Attr action -> [Html action] -> Html action
```


#### `label'`

``` purescript
label' :: forall action. [Html action] -> Html action
```


#### `legend`

``` purescript
legend :: forall action. Attr action -> [Html action] -> Html action
```


#### `legend'`

``` purescript
legend' :: forall action. [Html action] -> Html action
```


#### `li`

``` purescript
li :: forall action. Attr action -> [Html action] -> Html action
```


#### `li'`

``` purescript
li' :: forall action. [Html action] -> Html action
```


#### `link`

``` purescript
link :: forall action. Attr action -> [Html action] -> Html action
```


#### `link'`

``` purescript
link' :: forall action. [Html action] -> Html action
```


#### `main`

``` purescript
main :: forall action. Attr action -> [Html action] -> Html action
```


#### `main'`

``` purescript
main' :: forall action. [Html action] -> Html action
```


#### `map`

``` purescript
map :: forall action. Attr action -> [Html action] -> Html action
```


#### `map'`

``` purescript
map' :: forall action. [Html action] -> Html action
```


#### `mark`

``` purescript
mark :: forall action. Attr action -> [Html action] -> Html action
```


#### `mark'`

``` purescript
mark' :: forall action. [Html action] -> Html action
```


#### `menu`

``` purescript
menu :: forall action. Attr action -> [Html action] -> Html action
```


#### `menu'`

``` purescript
menu' :: forall action. [Html action] -> Html action
```


#### `menuitem`

``` purescript
menuitem :: forall action. Attr action -> [Html action] -> Html action
```


#### `menuitem'`

``` purescript
menuitem' :: forall action. [Html action] -> Html action
```


#### `meta`

``` purescript
meta :: forall action. Attr action -> [Html action] -> Html action
```


#### `meta'`

``` purescript
meta' :: forall action. [Html action] -> Html action
```


#### `meter`

``` purescript
meter :: forall action. Attr action -> [Html action] -> Html action
```


#### `meter'`

``` purescript
meter' :: forall action. [Html action] -> Html action
```


#### `nav`

``` purescript
nav :: forall action. Attr action -> [Html action] -> Html action
```


#### `nav'`

``` purescript
nav' :: forall action. [Html action] -> Html action
```


#### `noscript`

``` purescript
noscript :: forall action. Attr action -> [Html action] -> Html action
```


#### `noscript'`

``` purescript
noscript' :: forall action. [Html action] -> Html action
```


#### `object`

``` purescript
object :: forall action. Attr action -> [Html action] -> Html action
```


#### `object'`

``` purescript
object' :: forall action. [Html action] -> Html action
```


#### `ol`

``` purescript
ol :: forall action. Attr action -> [Html action] -> Html action
```


#### `ol'`

``` purescript
ol' :: forall action. [Html action] -> Html action
```


#### `optgroup`

``` purescript
optgroup :: forall action. Attr action -> [Html action] -> Html action
```


#### `optgroup'`

``` purescript
optgroup' :: forall action. [Html action] -> Html action
```


#### `option`

``` purescript
option :: forall action. Attr action -> [Html action] -> Html action
```


#### `option'`

``` purescript
option' :: forall action. [Html action] -> Html action
```


#### `output`

``` purescript
output :: forall action. Attr action -> [Html action] -> Html action
```


#### `output'`

``` purescript
output' :: forall action. [Html action] -> Html action
```


#### `p`

``` purescript
p :: forall action. Attr action -> [Html action] -> Html action
```


#### `p'`

``` purescript
p' :: forall action. [Html action] -> Html action
```


#### `param`

``` purescript
param :: forall action. Attr action -> [Html action] -> Html action
```


#### `param'`

``` purescript
param' :: forall action. [Html action] -> Html action
```


#### `picture`

``` purescript
picture :: forall action. Attr action -> [Html action] -> Html action
```


#### `picture'`

``` purescript
picture' :: forall action. [Html action] -> Html action
```


#### `pre`

``` purescript
pre :: forall action. Attr action -> [Html action] -> Html action
```


#### `pre'`

``` purescript
pre' :: forall action. [Html action] -> Html action
```


#### `progress`

``` purescript
progress :: forall action. Attr action -> [Html action] -> Html action
```


#### `progress'`

``` purescript
progress' :: forall action. [Html action] -> Html action
```


#### `q`

``` purescript
q :: forall action. Attr action -> [Html action] -> Html action
```


#### `q'`

``` purescript
q' :: forall action. [Html action] -> Html action
```


#### `rp`

``` purescript
rp :: forall action. Attr action -> [Html action] -> Html action
```


#### `rp'`

``` purescript
rp' :: forall action. [Html action] -> Html action
```


#### `rt`

``` purescript
rt :: forall action. Attr action -> [Html action] -> Html action
```


#### `rt'`

``` purescript
rt' :: forall action. [Html action] -> Html action
```


#### `ruby`

``` purescript
ruby :: forall action. Attr action -> [Html action] -> Html action
```


#### `ruby'`

``` purescript
ruby' :: forall action. [Html action] -> Html action
```


#### `s`

``` purescript
s :: forall action. Attr action -> [Html action] -> Html action
```


#### `s'`

``` purescript
s' :: forall action. [Html action] -> Html action
```


#### `samp`

``` purescript
samp :: forall action. Attr action -> [Html action] -> Html action
```


#### `samp'`

``` purescript
samp' :: forall action. [Html action] -> Html action
```


#### `script`

``` purescript
script :: forall action. Attr action -> [Html action] -> Html action
```


#### `script'`

``` purescript
script' :: forall action. [Html action] -> Html action
```


#### `section`

``` purescript
section :: forall action. Attr action -> [Html action] -> Html action
```


#### `section'`

``` purescript
section' :: forall action. [Html action] -> Html action
```


#### `select`

``` purescript
select :: forall action. Attr action -> [Html action] -> Html action
```


#### `select'`

``` purescript
select' :: forall action. [Html action] -> Html action
```


#### `small`

``` purescript
small :: forall action. Attr action -> [Html action] -> Html action
```


#### `small'`

``` purescript
small' :: forall action. [Html action] -> Html action
```


#### `source`

``` purescript
source :: forall action. Attr action -> [Html action] -> Html action
```


#### `source'`

``` purescript
source' :: forall action. [Html action] -> Html action
```


#### `span`

``` purescript
span :: forall action. Attr action -> [Html action] -> Html action
```


#### `span'`

``` purescript
span' :: forall action. [Html action] -> Html action
```


#### `strong`

``` purescript
strong :: forall action. Attr action -> [Html action] -> Html action
```


#### `strong'`

``` purescript
strong' :: forall action. [Html action] -> Html action
```


#### `style`

``` purescript
style :: forall action. Attr action -> [Html action] -> Html action
```


#### `style'`

``` purescript
style' :: forall action. [Html action] -> Html action
```


#### `sub`

``` purescript
sub :: forall action. Attr action -> [Html action] -> Html action
```


#### `sub'`

``` purescript
sub' :: forall action. [Html action] -> Html action
```


#### `summary`

``` purescript
summary :: forall action. Attr action -> [Html action] -> Html action
```


#### `summary'`

``` purescript
summary' :: forall action. [Html action] -> Html action
```


#### `sup`

``` purescript
sup :: forall action. Attr action -> [Html action] -> Html action
```


#### `sup'`

``` purescript
sup' :: forall action. [Html action] -> Html action
```


#### `table`

``` purescript
table :: forall action. Attr action -> [Html action] -> Html action
```


#### `table'`

``` purescript
table' :: forall action. [Html action] -> Html action
```


#### `tbody`

``` purescript
tbody :: forall action. Attr action -> [Html action] -> Html action
```


#### `tbody'`

``` purescript
tbody' :: forall action. [Html action] -> Html action
```


#### `td`

``` purescript
td :: forall action. Attr action -> [Html action] -> Html action
```


#### `td'`

``` purescript
td' :: forall action. [Html action] -> Html action
```


#### `textarea`

``` purescript
textarea :: forall action. Attr action -> [Html action] -> Html action
```


#### `textarea'`

``` purescript
textarea' :: forall action. [Html action] -> Html action
```


#### `tfoot`

``` purescript
tfoot :: forall action. Attr action -> [Html action] -> Html action
```


#### `tfoot'`

``` purescript
tfoot' :: forall action. [Html action] -> Html action
```


#### `th`

``` purescript
th :: forall action. Attr action -> [Html action] -> Html action
```


#### `th'`

``` purescript
th' :: forall action. [Html action] -> Html action
```


#### `thead`

``` purescript
thead :: forall action. Attr action -> [Html action] -> Html action
```


#### `thead'`

``` purescript
thead' :: forall action. [Html action] -> Html action
```


#### `time`

``` purescript
time :: forall action. Attr action -> [Html action] -> Html action
```


#### `time'`

``` purescript
time' :: forall action. [Html action] -> Html action
```


#### `title`

``` purescript
title :: forall action. Attr action -> [Html action] -> Html action
```


#### `title'`

``` purescript
title' :: forall action. [Html action] -> Html action
```


#### `tr`

``` purescript
tr :: forall action. Attr action -> [Html action] -> Html action
```


#### `tr'`

``` purescript
tr' :: forall action. [Html action] -> Html action
```


#### `track`

``` purescript
track :: forall action. Attr action -> [Html action] -> Html action
```


#### `track'`

``` purescript
track' :: forall action. [Html action] -> Html action
```


#### `u`

``` purescript
u :: forall action. Attr action -> [Html action] -> Html action
```


#### `u'`

``` purescript
u' :: forall action. [Html action] -> Html action
```


#### `ul`

``` purescript
ul :: forall action. Attr action -> [Html action] -> Html action
```


#### `ul'`

``` purescript
ul' :: forall action. [Html action] -> Html action
```


#### `var`

``` purescript
var :: forall action. Attr action -> [Html action] -> Html action
```


#### `var'`

``` purescript
var' :: forall action. [Html action] -> Html action
```


#### `video`

``` purescript
video :: forall action. Attr action -> [Html action] -> Html action
```


#### `video'`

``` purescript
video' :: forall action. [Html action] -> Html action
```


#### `wbr`

``` purescript
wbr :: forall action. Attr action -> [Html action] -> Html action
```


#### `wbr'`

``` purescript
wbr' :: forall action. [Html action] -> Html action
```



## Module Thermite.Events


This module defines helper functions for creating event handlers.

#### `ClipboardEvent`

``` purescript
data ClipboardEvent :: *
```


#### `onCopy`

``` purescript
onCopy :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Attr action
```


#### `onCut`

``` purescript
onCut :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Attr action
```


#### `onPaste`

``` purescript
onPaste :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Attr action
```


#### `KeyboardEvent`

``` purescript
data KeyboardEvent :: *
```


#### `onKeyDown`

``` purescript
onKeyDown :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Attr action
```


#### `onKeyPress`

``` purescript
onKeyPress :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Attr action
```


#### `onKeyUp`

``` purescript
onKeyUp :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Attr action
```


#### `FocusEvent`

``` purescript
data FocusEvent :: *
```


#### `onFocus`

``` purescript
onFocus :: forall state props action. Context state props action -> (FocusEvent -> action) -> Attr action
```


#### `onBlur`

``` purescript
onBlur :: forall state props action. Context state props action -> (FocusEvent -> action) -> Attr action
```


#### `FormEvent`

``` purescript
data FormEvent :: *
```


#### `onChange`

``` purescript
onChange :: forall state props action. Context state props action -> (FormEvent -> action) -> Attr action
```


#### `onInput`

``` purescript
onInput :: forall state props action. Context state props action -> (FormEvent -> action) -> Attr action
```


#### `onSubmit`

``` purescript
onSubmit :: forall state props action. Context state props action -> (FormEvent -> action) -> Attr action
```


#### `MouseEvent`

``` purescript
data MouseEvent :: *
```


#### `onClick`

``` purescript
onClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDoubleClick`

``` purescript
onDoubleClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDrag`

``` purescript
onDrag :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDragEnd`

``` purescript
onDragEnd :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDragEnter`

``` purescript
onDragEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDragExit`

``` purescript
onDragExit :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDragLeave`

``` purescript
onDragLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDragOver`

``` purescript
onDragOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDragStart`

``` purescript
onDragStart :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onDrop`

``` purescript
onDrop :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onMouseDown`

``` purescript
onMouseDown :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onMouseEnter`

``` purescript
onMouseEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onMouseLeave`

``` purescript
onMouseLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onMouseMove`

``` purescript
onMouseMove :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onMouseOut`

``` purescript
onMouseOut :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onMouseOver`

``` purescript
onMouseOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `onMouseUp`

``` purescript
onMouseUp :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
```


#### `TouchEvent`

``` purescript
data TouchEvent :: *
```


#### `onTouchCancel`

``` purescript
onTouchCancel :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
```


#### `onTouchEnd`

``` purescript
onTouchEnd :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
```


#### `onTouchMove`

``` purescript
onTouchMove :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
```


#### `onTouchStart`

``` purescript
onTouchStart :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
```


#### `UIEvent`

``` purescript
data UIEvent :: *
```


#### `onScroll`

``` purescript
onScroll :: forall state props action. Context state props action -> (UIEvent -> action) -> Attr action
```


#### `WheelEvent`

``` purescript
data WheelEvent :: *
```


#### `onWheel`

``` purescript
onWheel :: forall state props action. Context state props action -> (WheelEvent -> action) -> Attr action
```