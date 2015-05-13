# Module Documentation

## Module Thermite


This module defines functions for working with React components at a high level:

- `createClass` and `simpleSpec`, which can be used to create a component class
- `render` and `renderTo`, which can be used to render a component class

#### `PerformAction`

``` purescript
type PerformAction eff state props action = props -> action -> Action eff state Unit
```

A type synonym for action handlers, which take an action and the current properties
for the component, and return a computation in the `Action` monad.

#### `Render`

``` purescript
type Render eff state props action = Context state action -> state -> props -> [Html eff] -> Html eff
```

A rendering function, which takes a `Context`, the current state and properties, an array
of child nodes and returns a HTML document.

#### `Spec`

``` purescript
newtype Spec eff state props action
```

A component specification, which can be passed to `createClass`.

A minimal `Spec` can be built using `simpleSpec`, and extended with optional arguments
using functions in the `Thermite` module.

#### `SpecRecord`

``` purescript
type SpecRecord eff state props action = { displayName :: Maybe String, componentWillMount :: Maybe action, render :: Render eff state props action, performAction :: PerformAction eff state props action, initialState :: state }
```

A type synonym for the record type which captures the functions which make up a `Spec`.

#### `simpleSpec`

``` purescript
simpleSpec :: forall eff state props action. state -> PerformAction eff state props action -> Render eff state props action -> Spec eff state props action
```

Create a minimal `Spec`. The arguments are, in order:

- The initial component state
- A function for performing actions
- A function for rendering the current state as a HTML document

A `Spec` created using this function can be extended with optional properties using other functions
in this module.

#### `componentWillMount`

``` purescript
componentWillMount :: forall eff state props action. action -> Spec eff state props action -> Spec eff state props action
```

Extend a `Spec` with an action to run when the component will be mounted.

#### `displayName`

``` purescript
displayName :: forall eff state props action. String -> Spec eff state props action -> Spec eff state props action
```

Extend a `Spec` with a display name.

#### `createClass`

``` purescript
createClass :: forall eff state props action. Spec eff state props action -> ComponentClass props eff
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
data Context state action
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
data Attr
```

The type of HTML attributes.

#### `Html`

``` purescript
data Html (eff :: # !)
```

The type of HTML elements.

#### `semigroupAttr`

``` purescript
instance semigroupAttr :: Semigroup Attr
```


#### `monoidAttr`

``` purescript
instance monoidAttr :: Monoid Attr
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
runAction :: forall eff state props action a. Context state action -> Action eff state a -> Eff eff Unit
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
text :: forall eff. String -> Html eff
```

Create a text node.

#### `createElement`

``` purescript
createElement :: forall eff. String -> Attr -> [Html eff] -> Html eff
```

Create a HTML element from a tag name, a set of attributes and a collection of child nodes.

#### `component`

``` purescript
component :: forall props eff eff. ComponentClass props eff -> props -> [Html eff] -> Html eff
```

Create a HTML document from a component class


## Module Thermite.Html.Attributes


This module defines helper functions for creating HTML attributes.

#### `accept`

``` purescript
accept :: String -> Attr
```


#### `acceptCharset`

``` purescript
acceptCharset :: String -> Attr
```


#### `accessKey`

``` purescript
accessKey :: String -> Attr
```


#### `action`

``` purescript
action :: String -> Attr
```


#### `allowFullScreen`

``` purescript
allowFullScreen :: String -> Attr
```


#### `allowTransparency`

``` purescript
allowTransparency :: String -> Attr
```


#### `alt`

``` purescript
alt :: String -> Attr
```


#### `async`

``` purescript
async :: String -> Attr
```


#### `autoComplete`

``` purescript
autoComplete :: String -> Attr
```


#### `autoFocus`

``` purescript
autoFocus :: Boolean -> Attr
```


#### `autoPlay`

``` purescript
autoPlay :: String -> Attr
```


#### `cellPadding`

``` purescript
cellPadding :: String -> Attr
```


#### `cellSpacing`

``` purescript
cellSpacing :: String -> Attr
```


#### `charSet`

``` purescript
charSet :: String -> Attr
```


#### `checked`

``` purescript
checked :: String -> Attr
```


#### `classID`

``` purescript
classID :: String -> Attr
```


#### `className`

``` purescript
className :: String -> Attr
```


#### `cols`

``` purescript
cols :: String -> Attr
```


#### `colSpan`

``` purescript
colSpan :: String -> Attr
```


#### `content`

``` purescript
content :: String -> Attr
```


#### `contentEditable`

``` purescript
contentEditable :: String -> Attr
```


#### `contextMenu`

``` purescript
contextMenu :: String -> Attr
```


#### `controls`

``` purescript
controls :: String -> Attr
```


#### `coords`

``` purescript
coords :: String -> Attr
```


#### `crossOrigin`

``` purescript
crossOrigin :: String -> Attr
```


#### `dateTime`

``` purescript
dateTime :: String -> Attr
```


#### `defer`

``` purescript
defer :: String -> Attr
```


#### `dir`

``` purescript
dir :: String -> Attr
```


#### `disabled`

``` purescript
disabled :: Boolean -> Attr
```


#### `download`

``` purescript
download :: String -> Attr
```


#### `draggable`

``` purescript
draggable :: String -> Attr
```


#### `encType`

``` purescript
encType :: String -> Attr
```


#### `form`

``` purescript
form :: String -> Attr
```


#### `formAction`

``` purescript
formAction :: String -> Attr
```


#### `formEncType`

``` purescript
formEncType :: String -> Attr
```


#### `formMethod`

``` purescript
formMethod :: String -> Attr
```


#### `formNoValidate`

``` purescript
formNoValidate :: String -> Attr
```


#### `formTarget`

``` purescript
formTarget :: String -> Attr
```


#### `frameBorder`

``` purescript
frameBorder :: String -> Attr
```


#### `height`

``` purescript
height :: String -> Attr
```


#### `hidden`

``` purescript
hidden :: String -> Attr
```


#### `href`

``` purescript
href :: String -> Attr
```


#### `hrefLang`

``` purescript
hrefLang :: String -> Attr
```


#### `htmlFor`

``` purescript
htmlFor :: String -> Attr
```


#### `httpEquiv`

``` purescript
httpEquiv :: String -> Attr
```


#### `icon`

``` purescript
icon :: String -> Attr
```


#### `_id`

``` purescript
_id :: String -> Attr
```


#### `key`

``` purescript
key :: String -> Attr
```


#### `label`

``` purescript
label :: String -> Attr
```


#### `lang`

``` purescript
lang :: String -> Attr
```


#### `list`

``` purescript
list :: String -> Attr
```


#### `loop`

``` purescript
loop :: String -> Attr
```


#### `manifest`

``` purescript
manifest :: String -> Attr
```


#### `marginHeight`

``` purescript
marginHeight :: String -> Attr
```


#### `marginWidth`

``` purescript
marginWidth :: String -> Attr
```


#### `max`

``` purescript
max :: String -> Attr
```


#### `maxLength`

``` purescript
maxLength :: String -> Attr
```


#### `media`

``` purescript
media :: String -> Attr
```


#### `mediaGroup`

``` purescript
mediaGroup :: String -> Attr
```


#### `method`

``` purescript
method :: String -> Attr
```


#### `min`

``` purescript
min :: String -> Attr
```


#### `multiple`

``` purescript
multiple :: String -> Attr
```


#### `muted`

``` purescript
muted :: String -> Attr
```


#### `name`

``` purescript
name :: String -> Attr
```


#### `noValidate`

``` purescript
noValidate :: String -> Attr
```


#### `open`

``` purescript
open :: String -> Attr
```


#### `pattern`

``` purescript
pattern :: String -> Attr
```


#### `placeholder`

``` purescript
placeholder :: String -> Attr
```


#### `poster`

``` purescript
poster :: String -> Attr
```


#### `preload`

``` purescript
preload :: String -> Attr
```


#### `radioGroup`

``` purescript
radioGroup :: String -> Attr
```


#### `readOnly`

``` purescript
readOnly :: String -> Attr
```


#### `rel`

``` purescript
rel :: String -> Attr
```


#### `required`

``` purescript
required :: String -> Attr
```


#### `role`

``` purescript
role :: String -> Attr
```


#### `rows`

``` purescript
rows :: String -> Attr
```


#### `rowSpan`

``` purescript
rowSpan :: String -> Attr
```


#### `sandbox`

``` purescript
sandbox :: String -> Attr
```


#### `scope`

``` purescript
scope :: String -> Attr
```


#### `scrolling`

``` purescript
scrolling :: String -> Attr
```


#### `seamless`

``` purescript
seamless :: String -> Attr
```


#### `selected`

``` purescript
selected :: String -> Attr
```


#### `shape`

``` purescript
shape :: String -> Attr
```


#### `size`

``` purescript
size :: String -> Attr
```


#### `sizes`

``` purescript
sizes :: String -> Attr
```


#### `span`

``` purescript
span :: String -> Attr
```


#### `spellCheck`

``` purescript
spellCheck :: String -> Attr
```


#### `src`

``` purescript
src :: String -> Attr
```


#### `srcDoc`

``` purescript
srcDoc :: String -> Attr
```


#### `srcSet`

``` purescript
srcSet :: String -> Attr
```


#### `start`

``` purescript
start :: String -> Attr
```


#### `step`

``` purescript
step :: String -> Attr
```


#### `tabIndex`

``` purescript
tabIndex :: String -> Attr
```


#### `target`

``` purescript
target :: String -> Attr
```


#### `title`

``` purescript
title :: String -> Attr
```


#### `_type`

``` purescript
_type :: String -> Attr
```


#### `useMap`

``` purescript
useMap :: String -> Attr
```


#### `value`

``` purescript
value :: String -> Attr
```


#### `width`

``` purescript
width :: String -> Attr
```


#### `wmode`

``` purescript
wmode :: String -> Attr
```



## Module Thermite.Html.Elements


This module defines helper functions for creating HTML elements.

#### `a`

``` purescript
a :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `a'`

``` purescript
a' :: forall eff. [Html eff] -> Html eff
```


#### `abbr`

``` purescript
abbr :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `abbr'`

``` purescript
abbr' :: forall eff. [Html eff] -> Html eff
```


#### `address`

``` purescript
address :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `address'`

``` purescript
address' :: forall eff. [Html eff] -> Html eff
```


#### `area`

``` purescript
area :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `area'`

``` purescript
area' :: forall eff. [Html eff] -> Html eff
```


#### `article`

``` purescript
article :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `article'`

``` purescript
article' :: forall eff. [Html eff] -> Html eff
```


#### `aside`

``` purescript
aside :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `aside'`

``` purescript
aside' :: forall eff. [Html eff] -> Html eff
```


#### `audio`

``` purescript
audio :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `audio'`

``` purescript
audio' :: forall eff. [Html eff] -> Html eff
```


#### `b`

``` purescript
b :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `b'`

``` purescript
b' :: forall eff. [Html eff] -> Html eff
```


#### `base`

``` purescript
base :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `base'`

``` purescript
base' :: forall eff. [Html eff] -> Html eff
```


#### `bdi`

``` purescript
bdi :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `bdi'`

``` purescript
bdi' :: forall eff. [Html eff] -> Html eff
```


#### `bdo`

``` purescript
bdo :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `bdo'`

``` purescript
bdo' :: forall eff. [Html eff] -> Html eff
```


#### `big`

``` purescript
big :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `big'`

``` purescript
big' :: forall eff. [Html eff] -> Html eff
```


#### `blockquote`

``` purescript
blockquote :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `blockquote'`

``` purescript
blockquote' :: forall eff. [Html eff] -> Html eff
```


#### `body`

``` purescript
body :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `body'`

``` purescript
body' :: forall eff. [Html eff] -> Html eff
```


#### `br`

``` purescript
br :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `br'`

``` purescript
br' :: forall eff. [Html eff] -> Html eff
```


#### `button`

``` purescript
button :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `button'`

``` purescript
button' :: forall eff. [Html eff] -> Html eff
```


#### `canvas`

``` purescript
canvas :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `canvas'`

``` purescript
canvas' :: forall eff. [Html eff] -> Html eff
```


#### `caption`

``` purescript
caption :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `caption'`

``` purescript
caption' :: forall eff. [Html eff] -> Html eff
```


#### `cite`

``` purescript
cite :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `cite'`

``` purescript
cite' :: forall eff. [Html eff] -> Html eff
```


#### `code`

``` purescript
code :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `code'`

``` purescript
code' :: forall eff. [Html eff] -> Html eff
```


#### `col`

``` purescript
col :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `col'`

``` purescript
col' :: forall eff. [Html eff] -> Html eff
```


#### `colgroup`

``` purescript
colgroup :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `colgroup'`

``` purescript
colgroup' :: forall eff. [Html eff] -> Html eff
```


#### `_data`

``` purescript
_data :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `_data'`

``` purescript
_data' :: forall eff. [Html eff] -> Html eff
```


#### `datalist`

``` purescript
datalist :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `datalist'`

``` purescript
datalist' :: forall eff. [Html eff] -> Html eff
```


#### `dd`

``` purescript
dd :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `dd'`

``` purescript
dd' :: forall eff. [Html eff] -> Html eff
```


#### `del`

``` purescript
del :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `del'`

``` purescript
del' :: forall eff. [Html eff] -> Html eff
```


#### `details`

``` purescript
details :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `details'`

``` purescript
details' :: forall eff. [Html eff] -> Html eff
```


#### `dfn`

``` purescript
dfn :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `dfn'`

``` purescript
dfn' :: forall eff. [Html eff] -> Html eff
```


#### `dialog`

``` purescript
dialog :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `dialog'`

``` purescript
dialog' :: forall eff. [Html eff] -> Html eff
```


#### `div`

``` purescript
div :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `div'`

``` purescript
div' :: forall eff. [Html eff] -> Html eff
```


#### `dl`

``` purescript
dl :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `dl'`

``` purescript
dl' :: forall eff. [Html eff] -> Html eff
```


#### `dt`

``` purescript
dt :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `dt'`

``` purescript
dt' :: forall eff. [Html eff] -> Html eff
```


#### `em`

``` purescript
em :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `em'`

``` purescript
em' :: forall eff. [Html eff] -> Html eff
```


#### `embed`

``` purescript
embed :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `embed'`

``` purescript
embed' :: forall eff. [Html eff] -> Html eff
```


#### `fieldset`

``` purescript
fieldset :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `fieldset'`

``` purescript
fieldset' :: forall eff. [Html eff] -> Html eff
```


#### `figcaption`

``` purescript
figcaption :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `figcaption'`

``` purescript
figcaption' :: forall eff. [Html eff] -> Html eff
```


#### `figure`

``` purescript
figure :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `figure'`

``` purescript
figure' :: forall eff. [Html eff] -> Html eff
```


#### `footer`

``` purescript
footer :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `footer'`

``` purescript
footer' :: forall eff. [Html eff] -> Html eff
```


#### `form`

``` purescript
form :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `form'`

``` purescript
form' :: forall eff. [Html eff] -> Html eff
```


#### `h1`

``` purescript
h1 :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `h1'`

``` purescript
h1' :: forall eff. [Html eff] -> Html eff
```


#### `h2`

``` purescript
h2 :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `h2'`

``` purescript
h2' :: forall eff. [Html eff] -> Html eff
```


#### `h3`

``` purescript
h3 :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `h3'`

``` purescript
h3' :: forall eff. [Html eff] -> Html eff
```


#### `h4`

``` purescript
h4 :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `h4'`

``` purescript
h4' :: forall eff. [Html eff] -> Html eff
```


#### `h5`

``` purescript
h5 :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `h5'`

``` purescript
h5' :: forall eff. [Html eff] -> Html eff
```


#### `h6`

``` purescript
h6 :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `h6'`

``` purescript
h6' :: forall eff. [Html eff] -> Html eff
```


#### `head`

``` purescript
head :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `head'`

``` purescript
head' :: forall eff. [Html eff] -> Html eff
```


#### `header`

``` purescript
header :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `header'`

``` purescript
header' :: forall eff. [Html eff] -> Html eff
```


#### `hr`

``` purescript
hr :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `hr'`

``` purescript
hr' :: forall eff. [Html eff] -> Html eff
```


#### `html`

``` purescript
html :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `html'`

``` purescript
html' :: forall eff. [Html eff] -> Html eff
```


#### `i`

``` purescript
i :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `i'`

``` purescript
i' :: forall eff. [Html eff] -> Html eff
```


#### `iframe`

``` purescript
iframe :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `iframe'`

``` purescript
iframe' :: forall eff. [Html eff] -> Html eff
```


#### `img`

``` purescript
img :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `img'`

``` purescript
img' :: forall eff. [Html eff] -> Html eff
```


#### `input`

``` purescript
input :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `input'`

``` purescript
input' :: forall eff. [Html eff] -> Html eff
```


#### `ins`

``` purescript
ins :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `ins'`

``` purescript
ins' :: forall eff. [Html eff] -> Html eff
```


#### `kbd`

``` purescript
kbd :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `kbd'`

``` purescript
kbd' :: forall eff. [Html eff] -> Html eff
```


#### `keygen`

``` purescript
keygen :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `keygen'`

``` purescript
keygen' :: forall eff. [Html eff] -> Html eff
```


#### `label`

``` purescript
label :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `label'`

``` purescript
label' :: forall eff. [Html eff] -> Html eff
```


#### `legend`

``` purescript
legend :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `legend'`

``` purescript
legend' :: forall eff. [Html eff] -> Html eff
```


#### `li`

``` purescript
li :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `li'`

``` purescript
li' :: forall eff. [Html eff] -> Html eff
```


#### `link`

``` purescript
link :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `link'`

``` purescript
link' :: forall eff. [Html eff] -> Html eff
```


#### `main`

``` purescript
main :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `main'`

``` purescript
main' :: forall eff. [Html eff] -> Html eff
```


#### `map`

``` purescript
map :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `map'`

``` purescript
map' :: forall eff. [Html eff] -> Html eff
```


#### `mark`

``` purescript
mark :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `mark'`

``` purescript
mark' :: forall eff. [Html eff] -> Html eff
```


#### `menu`

``` purescript
menu :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `menu'`

``` purescript
menu' :: forall eff. [Html eff] -> Html eff
```


#### `menuitem`

``` purescript
menuitem :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `menuitem'`

``` purescript
menuitem' :: forall eff. [Html eff] -> Html eff
```


#### `meta`

``` purescript
meta :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `meta'`

``` purescript
meta' :: forall eff. [Html eff] -> Html eff
```


#### `meter`

``` purescript
meter :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `meter'`

``` purescript
meter' :: forall eff. [Html eff] -> Html eff
```


#### `nav`

``` purescript
nav :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `nav'`

``` purescript
nav' :: forall eff. [Html eff] -> Html eff
```


#### `noscript`

``` purescript
noscript :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `noscript'`

``` purescript
noscript' :: forall eff. [Html eff] -> Html eff
```


#### `object`

``` purescript
object :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `object'`

``` purescript
object' :: forall eff. [Html eff] -> Html eff
```


#### `ol`

``` purescript
ol :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `ol'`

``` purescript
ol' :: forall eff. [Html eff] -> Html eff
```


#### `optgroup`

``` purescript
optgroup :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `optgroup'`

``` purescript
optgroup' :: forall eff. [Html eff] -> Html eff
```


#### `option`

``` purescript
option :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `option'`

``` purescript
option' :: forall eff. [Html eff] -> Html eff
```


#### `output`

``` purescript
output :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `output'`

``` purescript
output' :: forall eff. [Html eff] -> Html eff
```


#### `p`

``` purescript
p :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `p'`

``` purescript
p' :: forall eff. [Html eff] -> Html eff
```


#### `param`

``` purescript
param :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `param'`

``` purescript
param' :: forall eff. [Html eff] -> Html eff
```


#### `picture`

``` purescript
picture :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `picture'`

``` purescript
picture' :: forall eff. [Html eff] -> Html eff
```


#### `pre`

``` purescript
pre :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `pre'`

``` purescript
pre' :: forall eff. [Html eff] -> Html eff
```


#### `progress`

``` purescript
progress :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `progress'`

``` purescript
progress' :: forall eff. [Html eff] -> Html eff
```


#### `q`

``` purescript
q :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `q'`

``` purescript
q' :: forall eff. [Html eff] -> Html eff
```


#### `rp`

``` purescript
rp :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `rp'`

``` purescript
rp' :: forall eff. [Html eff] -> Html eff
```


#### `rt`

``` purescript
rt :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `rt'`

``` purescript
rt' :: forall eff. [Html eff] -> Html eff
```


#### `ruby`

``` purescript
ruby :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `ruby'`

``` purescript
ruby' :: forall eff. [Html eff] -> Html eff
```


#### `s`

``` purescript
s :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `s'`

``` purescript
s' :: forall eff. [Html eff] -> Html eff
```


#### `samp`

``` purescript
samp :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `samp'`

``` purescript
samp' :: forall eff. [Html eff] -> Html eff
```


#### `script`

``` purescript
script :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `script'`

``` purescript
script' :: forall eff. [Html eff] -> Html eff
```


#### `section`

``` purescript
section :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `section'`

``` purescript
section' :: forall eff. [Html eff] -> Html eff
```


#### `select`

``` purescript
select :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `select'`

``` purescript
select' :: forall eff. [Html eff] -> Html eff
```


#### `small`

``` purescript
small :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `small'`

``` purescript
small' :: forall eff. [Html eff] -> Html eff
```


#### `source`

``` purescript
source :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `source'`

``` purescript
source' :: forall eff. [Html eff] -> Html eff
```


#### `span`

``` purescript
span :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `span'`

``` purescript
span' :: forall eff. [Html eff] -> Html eff
```


#### `strong`

``` purescript
strong :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `strong'`

``` purescript
strong' :: forall eff. [Html eff] -> Html eff
```


#### `style`

``` purescript
style :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `style'`

``` purescript
style' :: forall eff. [Html eff] -> Html eff
```


#### `sub`

``` purescript
sub :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `sub'`

``` purescript
sub' :: forall eff. [Html eff] -> Html eff
```


#### `summary`

``` purescript
summary :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `summary'`

``` purescript
summary' :: forall eff. [Html eff] -> Html eff
```


#### `sup`

``` purescript
sup :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `sup'`

``` purescript
sup' :: forall eff. [Html eff] -> Html eff
```


#### `table`

``` purescript
table :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `table'`

``` purescript
table' :: forall eff. [Html eff] -> Html eff
```


#### `tbody`

``` purescript
tbody :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `tbody'`

``` purescript
tbody' :: forall eff. [Html eff] -> Html eff
```


#### `td`

``` purescript
td :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `td'`

``` purescript
td' :: forall eff. [Html eff] -> Html eff
```


#### `textarea`

``` purescript
textarea :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `textarea'`

``` purescript
textarea' :: forall eff. [Html eff] -> Html eff
```


#### `tfoot`

``` purescript
tfoot :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `tfoot'`

``` purescript
tfoot' :: forall eff. [Html eff] -> Html eff
```


#### `th`

``` purescript
th :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `th'`

``` purescript
th' :: forall eff. [Html eff] -> Html eff
```


#### `thead`

``` purescript
thead :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `thead'`

``` purescript
thead' :: forall eff. [Html eff] -> Html eff
```


#### `time`

``` purescript
time :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `time'`

``` purescript
time' :: forall eff. [Html eff] -> Html eff
```


#### `title`

``` purescript
title :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `title'`

``` purescript
title' :: forall eff. [Html eff] -> Html eff
```


#### `tr`

``` purescript
tr :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `tr'`

``` purescript
tr' :: forall eff. [Html eff] -> Html eff
```


#### `track`

``` purescript
track :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `track'`

``` purescript
track' :: forall eff. [Html eff] -> Html eff
```


#### `u`

``` purescript
u :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `u'`

``` purescript
u' :: forall eff. [Html eff] -> Html eff
```


#### `ul`

``` purescript
ul :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `ul'`

``` purescript
ul' :: forall eff. [Html eff] -> Html eff
```


#### `var`

``` purescript
var :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `var'`

``` purescript
var' :: forall eff. [Html eff] -> Html eff
```


#### `video`

``` purescript
video :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `video'`

``` purescript
video' :: forall eff. [Html eff] -> Html eff
```


#### `wbr`

``` purescript
wbr :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `wbr'`

``` purescript
wbr' :: forall eff. [Html eff] -> Html eff
```



## Module Thermite.SVG


This module defines helper functions for creating SVG elements.

#### `circle`

``` purescript
circle :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `clipPath`

``` purescript
clipPath :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `defs`

``` purescript
defs :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `ellipse`

``` purescript
ellipse :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `g`

``` purescript
g :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `line`

``` purescript
line :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `linearGradient`

``` purescript
linearGradient :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `mask`

``` purescript
mask :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `path`

``` purescript
path :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `pattern`

``` purescript
pattern :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `polygon`

``` purescript
polygon :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `polyline`

``` purescript
polyline :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `radialGradient`

``` purescript
radialGradient :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `rect`

``` purescript
rect :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `stop`

``` purescript
stop :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `svg`

``` purescript
svg :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `text`

``` purescript
text :: forall eff. Attr -> [Html eff] -> Html eff
```


#### `tspan`

``` purescript
tspan :: forall eff. Attr -> [Html eff] -> Html eff
```



## Module Thermite.SVG.Attributes


This module defines helper functions for creating SVG attributes.

#### `clipPath`

``` purescript
clipPath :: String -> Attr
```


#### `cx`

``` purescript
cx :: String -> Attr
```


#### `cy`

``` purescript
cy :: String -> Attr
```


#### `d`

``` purescript
d :: String -> Attr
```


#### `dx`

``` purescript
dx :: String -> Attr
```


#### `dy`

``` purescript
dy :: String -> Attr
```


#### `fill`

``` purescript
fill :: String -> Attr
```


#### `fillOpacity`

``` purescript
fillOpacity :: String -> Attr
```


#### `fontFamily`

``` purescript
fontFamily :: String -> Attr
```


#### `fontSize`

``` purescript
fontSize :: String -> Attr
```


#### `fx`

``` purescript
fx :: String -> Attr
```


#### `fy`

``` purescript
fy :: String -> Attr
```


#### `gradientTransform`

``` purescript
gradientTransform :: String -> Attr
```


#### `gradientUnits`

``` purescript
gradientUnits :: String -> Attr
```


#### `markerEnd`

``` purescript
markerEnd :: String -> Attr
```


#### `markerMid`

``` purescript
markerMid :: String -> Attr
```


#### `markerStart`

``` purescript
markerStart :: String -> Attr
```


#### `offset`

``` purescript
offset :: String -> Attr
```


#### `opacity`

``` purescript
opacity :: String -> Attr
```


#### `patternContentUnits`

``` purescript
patternContentUnits :: String -> Attr
```


#### `patternUnits`

``` purescript
patternUnits :: String -> Attr
```


#### `points`

``` purescript
points :: String -> Attr
```


#### `preserveAspectRatio`

``` purescript
preserveAspectRatio :: String -> Attr
```


#### `r`

``` purescript
r :: String -> Attr
```


#### `rx`

``` purescript
rx :: String -> Attr
```


#### `ry`

``` purescript
ry :: String -> Attr
```


#### `spreadMethod`

``` purescript
spreadMethod :: String -> Attr
```


#### `stopColor`

``` purescript
stopColor :: String -> Attr
```


#### `stopOpacity`

``` purescript
stopOpacity :: String -> Attr
```


#### `stroke`

``` purescript
stroke :: String -> Attr
```


#### `strokeDasharray`

``` purescript
strokeDasharray :: String -> Attr
```


#### `strokeLinecap`

``` purescript
strokeLinecap :: String -> Attr
```


#### `strokeOpacity`

``` purescript
strokeOpacity :: String -> Attr
```


#### `strokeWidth`

``` purescript
strokeWidth :: String -> Attr
```


#### `textAnchor`

``` purescript
textAnchor :: String -> Attr
```


#### `transform`

``` purescript
transform :: String -> Attr
```


#### `version`

``` purescript
version :: String -> Attr
```


#### `viewBox`

``` purescript
viewBox :: String -> Attr
```


#### `x1`

``` purescript
x1 :: String -> Attr
```


#### `x2`

``` purescript
x2 :: String -> Attr
```


#### `x`

``` purescript
x :: String -> Attr
```


#### `y1`

``` purescript
y1 :: String -> Attr
```


#### `y2`

``` purescript
y2 :: String -> Attr
```


#### `y`

``` purescript
y :: String -> Attr
```



## Module Thermite.Events


This module defines helper functions for creating event handlers.

#### `ClipboardEvent`

``` purescript
data ClipboardEvent :: *
```


#### `onCopy`

``` purescript
onCopy :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
```


#### `onCut`

``` purescript
onCut :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
```


#### `onPaste`

``` purescript
onPaste :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
```


#### `KeyboardEvent`

``` purescript
data KeyboardEvent :: *
```


#### `onKeyDown`

``` purescript
onKeyDown :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
```


#### `onKeyPress`

``` purescript
onKeyPress :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
```


#### `onKeyUp`

``` purescript
onKeyUp :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
```


#### `FocusEvent`

``` purescript
data FocusEvent :: *
```


#### `onFocus`

``` purescript
onFocus :: forall state props action. Context state action -> (FocusEvent -> action) -> Attr
```


#### `onBlur`

``` purescript
onBlur :: forall state props action. Context state action -> (FocusEvent -> action) -> Attr
```


#### `FormEvent`

``` purescript
data FormEvent :: *
```


#### `onChange`

``` purescript
onChange :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
```


#### `onInput`

``` purescript
onInput :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
```


#### `onSubmit`

``` purescript
onSubmit :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
```


#### `MouseEvent`

``` purescript
data MouseEvent :: *
```


#### `onClick`

``` purescript
onClick :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDoubleClick`

``` purescript
onDoubleClick :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDrag`

``` purescript
onDrag :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDragEnd`

``` purescript
onDragEnd :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDragEnter`

``` purescript
onDragEnter :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDragExit`

``` purescript
onDragExit :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDragLeave`

``` purescript
onDragLeave :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDragOver`

``` purescript
onDragOver :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDragStart`

``` purescript
onDragStart :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onDrop`

``` purescript
onDrop :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onMouseDown`

``` purescript
onMouseDown :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onMouseEnter`

``` purescript
onMouseEnter :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onMouseLeave`

``` purescript
onMouseLeave :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onMouseMove`

``` purescript
onMouseMove :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onMouseOut`

``` purescript
onMouseOut :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onMouseOver`

``` purescript
onMouseOver :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `onMouseUp`

``` purescript
onMouseUp :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```


#### `TouchEvent`

``` purescript
data TouchEvent :: *
```


#### `onTouchCancel`

``` purescript
onTouchCancel :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```


#### `onTouchEnd`

``` purescript
onTouchEnd :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```


#### `onTouchMove`

``` purescript
onTouchMove :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```


#### `onTouchStart`

``` purescript
onTouchStart :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```


#### `UIEvent`

``` purescript
data UIEvent :: *
```


#### `onScroll`

``` purescript
onScroll :: forall state props action. Context state action -> (UIEvent -> action) -> Attr
```


#### `WheelEvent`

``` purescript
data WheelEvent :: *
```


#### `onWheel`

``` purescript
onWheel :: forall state props action. Context state action -> (WheelEvent -> action) -> Attr
```