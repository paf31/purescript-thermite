## Module Thermite

Thermite provides a simple model-view-action abstraction on top of `purescript-react`:

- A `Spec` defines a `state` type which acts as the _model_.
- The `Spec` also defines an `action` type which acts as the set of _actions_.
- The `view` is a `Render` function which produces a React element for the current state.
- The `PerformAction` function can be used to update the state based on an action.

A `Spec` can be created using its newtype constructor, and turned into a React component class using
`createClass`.

Thermite also provides type class instances and lens combinators for composing `Spec`s.

#### `PerformAction`

``` purescript
type PerformAction state props action = action -> props -> state -> CoTransformer (Maybe state) (state -> state) Aff Unit
```

A type synonym for an action handler, which takes an action, the current props
and state for the component, and return a `CoTransformer` which will emit
state updates asynchronously.

`Control.Coroutine.cotransform` can be used to emit state update functions
and wait for the new state value. If `cotransform` returns `Nothing`, then
the state could not be updated. Usually, this will not happen, but it is possible
in certain use cases involving `split` and `foreach`.

#### `defaultPerformAction`

``` purescript
defaultPerformAction :: forall state props action. PerformAction state props action
```

A default `PerformAction` action implementation which ignores all actions.

#### `Dispatch`

``` purescript
type Dispatch action = action -> Effect Unit
```

A function capable of dispatching an action type, invoking `PerformAction`.

#### `Render`

``` purescript
type Render state props action = Dispatch action -> props -> state -> Array ReactElement -> Array ReactElement
```

A rendering function, which takes an action handler function, the current state and
props, an array of child nodes, and returns a HTML document.

#### `defaultRender`

``` purescript
defaultRender :: forall state props action. Render state props action
```

A default `Render` implementation which renders nothing.

This is useful when just `append`ing action handlers.

#### `writeState`

``` purescript
writeState :: forall state. state -> CoTransformer (Maybe state) (state -> state) Aff (Maybe state)
```

Replace the current component state.

#### `modifyState`

``` purescript
modifyState :: forall state. (state -> state) -> CoTransformer (Maybe state) (state -> state) Aff (Maybe state)
```

An alias for `cotransform` - apply a function to the current component state.

#### `WithChildren`

``` purescript
type WithChildren props = { children :: Children | props }
```

Thermite assumes _all_ thermite-created components have children. This is a convenience type
when specifying the `children :: Children` prop field of a `Spec`.

#### `Spec`

``` purescript
newtype Spec state props action
  = Spec { performAction :: PerformAction state props action, render :: Render state props action }
```

A component specification, which can be passed to `createClass`.

For example:

```purescript
import qualified React.DOM as R

data Action = Increment

spec :: Spec Int _ Action
spec = Spec {performAction, render}
  where
  render :: Render Int _ Action
  render _ _ n _ = [ R.text (show n) ]

  performAction :: PerformAction Int _ Action
  performAction Increment _ n k = k (n + 1)
```

The `Monoid` instance for `Spec` will compose `Spec`s by placing rendered
HTML elements next to one another, and performing actions in sequence.

##### Instances
``` purescript
Semigroup (Spec state props action)
Monoid (Spec state props action)
```

#### `_performAction`

``` purescript
_performAction :: forall state props action. Lens' (Spec state props action) (PerformAction state props action)
```

A `Lens` for accessing the `PerformAction` portion of a `Spec`.

#### `_render`

``` purescript
_render :: forall state props action. Lens' (Spec state props action) (Render state props action)
```

A `Lens` for accessing the `Render` portion of a `Spec`.

This can be useful when wrapping a `Render` function in order to frame a
set of controls with some containing element. For example:

```purescript
wrap :: Spec State _ Action -> Spec State _ Action
wrap = over _render \child dispatch props state childre  ->
  [ R.div [ RP.className "wrapper" ] [ child dispatch props state children ] ]
```

#### `createClass`

``` purescript
createClass :: forall state props action. Spec {  | state } (WithChildren props) action -> {  | state } -> String -> ReactClass (WithChildren props)
```

Create a React component class from a Thermite component `Spec`.

#### `createReactConstructor`

``` purescript
createReactConstructor :: forall state props action. Spec {  | state } (WithChildren props) action -> {  | state } -> { constructor :: ReactClassConstructor (WithChildren props) {  | state } (ReactSpecRequired {  | state } ()), dispatcher :: ReactThis (WithChildren props) {  | state } -> Dispatch action }
```

Create a React component constructor from a Thermite component `Spec`.

This function is a low-level alternative to `createClass`, used when the React
component constructor needs to be modified before being turned into a component class,
e.g. by adding additional lifecycle methods.

__Note__: React assumes _all_ react components have a _record_-based state; when constructing
and composing pure Thermite `Spec`s, you are free to decide whichever state construct you wish.
However, when finally turning a `Spec` into to React-friendly code, it must be a `Record`.

#### `defaultMain`

``` purescript
defaultMain :: forall state props given action. ReactPropFields props given => Spec {  | state } (WithChildren props) action -> {  | state } -> String -> {  | given } -> Effect Unit
```

A default implementation of `main` which renders a component to the
document body.

#### `withState`

``` purescript
withState :: forall state props action. (state -> Spec state props action) -> Spec state props action
```

This function captures the state of the `Spec` as a function argument.

This can sometimes be useful in complex scenarios involving the `focus` and
`foreach` combinators.

#### `withProps`

``` purescript
withProps :: forall state props action. (props -> Spec state props action) -> Spec state props action
```

This function captures the props of the `Spec` as a function argument.

#### `focus`

``` purescript
focus :: forall props state2 state1 action1 action2. Lens' state2 state1 -> Prism' action2 action1 -> Spec state1 props action1 -> Spec state2 props action2
```

Change the state type, using a lens to focus on a part of the state.

For example, to combine two `Spec`s, combining state types using `Tuple`
and action types using `Either`:

```purescript
spec1 :: Spec S1 _ A1
spec2 :: Spec S2 _ A2

spec :: Spec (Tuple S1 S2) _ (Either A1 A2)
spec = focus _1 _Left spec1 <> focus _2 _Right spec2
```

Actions will only be handled when the prism matches its input, otherwise
the action will be ignored, and should be handled by some other component.

#### `focusState`

``` purescript
focusState :: forall props state2 state1 action. Lens' state2 state1 -> Spec state1 props action -> Spec state2 props action
```

A variant of `focus` which only changes the state type, by applying a `Lens`.

#### `match`

``` purescript
match :: forall props state action1 action2. Prism' action2 action1 -> Spec state props action1 -> Spec state props action2
```

A variant of `focus` which only changes the action type, by applying a `Prism`,
effectively matching some subset of a larger action type.

#### `split`

``` purescript
split :: forall props state1 state2 action. Prism' state1 state2 -> Spec state2 props action -> Spec state1 props action
```

Create a component which renders an optional subcomponent.

#### `foreach`

``` purescript
foreach :: forall props state action. (Int -> Spec state props action) -> Spec (Array state) props (Tuple Int action)
```

Create a component whose state is described by an array, displaying one subcomponent
for each entry in the list.

The action type is modified to take the index of the originating subcomponent as an
additional argument.

#### `cmapProps`

``` purescript
cmapProps :: forall state props props' action. (props' -> props) -> Spec state props action -> Spec state props' action
```


### Re-exported from Control.Coroutine:

#### `CoTransformer`

``` purescript
type CoTransformer i o = Co (CoTransform i o)
```

A type synonym for a `Co`routine which "cotransforms" values, emitting an output
before waiting for its input.

#### `cotransform`

``` purescript
cotransform :: forall m i o. Monad m => o -> CoTransformer i o m i
```

Cotransform input values.

