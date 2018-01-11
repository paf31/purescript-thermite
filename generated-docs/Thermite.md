## Module Thermite

Thermite provides a simple model-view-action abstraction on top of `purescript-react`:

- A `Spec` defines a `state` type which acts as the _model_.
- The `Spec` also defines an `action` type which acts as the set of _actions_.
- The `view` is a `Render` function which produces a React element for the current state.
- The `PerformAction` function can be used to update the state based on an action.

A `Spec` can be created using `simpleSpec`, and turned into a React component class using
`createClass`.

Thermite also provides type class instances and lens combinators for composing `Spec`s.

#### `PerformAction`

``` purescript
type PerformAction eff state props action = action -> props -> state -> CoTransformer (Maybe state) (state -> state) (Aff eff) Unit
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
defaultPerformAction :: forall eff state props action. PerformAction eff state props action
```

A default `PerformAction` action implementation which ignores all actions.

#### `writeState`

``` purescript
writeState :: forall state eff. state -> CoTransformer (Maybe state) (state -> state) (Aff eff) (Maybe state)
```

Replace the current component state.

#### `modifyState`

``` purescript
modifyState :: forall state eff. (state -> state) -> CoTransformer (Maybe state) (state -> state) (Aff eff) (Maybe state)
```

An alias for `cotransform` - apply a function to the current component state.

#### `EventHandler`

``` purescript
type EventHandler = forall eff refs. Eff (props :: ReactProps, state :: ReactState ReadWrite, refs :: ReactRefs refs | eff) Unit
```

A type synonym for an event handler which can be used to construct
`purescript-react`'s event attributes.

#### `Render`

``` purescript
type Render state props action = (action -> EventHandler) -> props -> state -> Array ReactElement -> Array ReactElement
```

A rendering function, which takes an action handler function, the current state and
props, an array of child nodes and returns a HTML document.

#### `defaultRender`

``` purescript
defaultRender :: forall state props action. Render state props action
```

A default `Render` implementation which renders nothing.

This is useful when just `append`ing action handlers.

#### `Spec`

``` purescript
newtype Spec eff state props action
```

A component specification, which can be passed to `createClass`.

A minimal `Spec` can be built using `simpleSpec`.

The `Monoid` instance for `Spec` will compose `Spec`s by placing rendered
HTML elements next to one another, and performing actions in sequence.

##### Instances
``` purescript
Semigroup (Spec eff state props action)
Monoid (Spec eff state props action)
```

#### `_performAction`

``` purescript
_performAction :: forall eff state props action. Lens' (Spec eff state props action) (PerformAction eff state props action)
```

A `Lens` for accessing the `PerformAction` portion of a `Spec`.

#### `_render`

``` purescript
_render :: forall eff state props action. Lens' (Spec eff state props action) (Render state props action)
```

A `Lens` for accessing the `Render` portion of a `Spec`.

This can be useful when wrapping a `Render` function in order to frame a
set of controls with some containing element. For example:

```purescript
wrap :: Spec _ State _ Action -> Spec _ State _ Action
wrap = over _render \child dispatch props state childre  ->
  [ R.div [ RP.className "wrapper" ] [ child dispatch props state children ] ]
```

#### `simpleSpec`

``` purescript
simpleSpec :: forall eff state props action. PerformAction eff state props action -> Render state props action -> Spec eff state props action
```

Create a minimal `Spec`. The arguments are, in order:

- The `PerformAction` function for performing actions
- The `Render` function for rendering the current state as a HTML document

For example:

```purescript
import qualified React.DOM as R

data Action = Increment

spec :: Spec _ Int _ Action
spec = simpleSpec performAction render
  where
  render :: Render _ Int _
  render _ _ n _ = [ R.text (show n) ]

  performAction :: PerformAction _ Int _ Action
  performAction Increment _ n k = k (n + 1)
```

#### `createClass`

``` purescript
createClass :: forall eff state props action. Spec eff state props action -> state -> ReactClass props
```

Create a React component class from a Thermite component `Spec`.

#### `createReactSpec`

``` purescript
createReactSpec :: forall eff state props action. Spec eff state props action -> state -> { spec :: ReactSpec props state eff, dispatcher :: ReactThis props state -> action -> EventHandler }
```

Create a React component spec from a Thermite component `Spec`.

This function is a low-level alternative to `createClass`, used when the React
component spec needs to be modified before being turned into a component class,
e.g. by adding additional lifecycle methods.

#### `createReactSpec'`

``` purescript
createReactSpec' :: forall eff state props action. (Array ReactElement -> ReactElement) -> Spec eff state props action -> state -> { spec :: ReactSpec props state eff, dispatcher :: ReactThis props state -> action -> EventHandler }
```

Create a React component spec from a Thermite component `Spec` with an additional
function for converting the rendered Array of ReactElement's into a single ReactElement
as is required by React.

This function is a low-level alternative to `createClass`, used when the React
component spec needs to be modified before being turned into a component class,
e.g. by adding additional lifecycle methods.

#### `defaultMain`

``` purescript
defaultMain :: forall state props action eff. Spec eff state props action -> state -> props -> Eff (dom :: DOM | eff) Unit
```

A default implementation of `main` which renders a component to the
document body.

#### `withState`

``` purescript
withState :: forall eff state props action. (state -> Spec eff state props action) -> Spec eff state props action
```

This function captures the state of the `Spec` as a function argument.

This can sometimes be useful in complex scenarios involving the `focus` and
`foreach` combinators.

#### `withProps`

``` purescript
withProps :: forall eff state props action. (props -> Spec eff state props action) -> Spec eff state props action
```

This function captures the props of the `Spec` as a function argument.

#### `focus`

``` purescript
focus :: forall eff props state2 state1 action1 action2. Lens' state2 state1 -> Prism' action2 action1 -> Spec eff state1 props action1 -> Spec eff state2 props action2
```

Change the state type, using a lens to focus on a part of the state.

For example, to combine two `Spec`s, combining state types using `Tuple`
and action types using `Either`:

```purescript
spec1 :: Spec _ S1 _ A1
spec2 :: Spec _ S2 _ A2

spec :: Spec _ (Tuple S1 S2) _ (Either A1 A2)
spec = focus _1 _Left spec1 <> focus _2 _Right spec2
```

Actions will only be handled when the prism matches its input, otherwise
the action will be ignored, and should be handled by some other component.

#### `focusState`

``` purescript
focusState :: forall eff props state2 state1 action. Lens' state2 state1 -> Spec eff state1 props action -> Spec eff state2 props action
```

A variant of `focus` which only changes the state type, by applying a `Lens`.

#### `match`

``` purescript
match :: forall eff props state action1 action2. Prism' action2 action1 -> Spec eff state props action1 -> Spec eff state props action2
```

A variant of `focus` which only changes the action type, by applying a `Prism`,
effectively matching some subset of a larger action type.

#### `split`

``` purescript
split :: forall eff props state1 state2 action. Prism' state1 state2 -> Spec eff state2 props action -> Spec eff state1 props action
```

Create a component which renders an optional subcomponent.

#### `foreach`

``` purescript
foreach :: forall eff props state action. (Int -> Spec eff state props action) -> Spec eff (List state) props (Tuple Int action)
```

Create a component whose state is described by a list, displaying one subcomponent
for each entry in the list.

The action type is modified to take the index of the originating subcomponent as an
additional argument.


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

