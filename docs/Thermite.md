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
type Render eff state props action = (action -> EventHandlerContext eff props state Unit) -> state -> props -> Array ReactElement -> ReactElement
```

A rendering function, which takes a event dispatcher function, the current state and
props, an arrayof child nodes and returns a HTML document.

#### `Spec`

``` purescript
newtype Spec eff state props action
```

A component specification, which can be passed to `createClass`.

A minimal `Spec` can be built using `simpleSpec`, and extended with optional arguments
using functions in the `Thermite` module.

#### `SpecRecord`

``` purescript
type SpecRecord eff state props action = { initialState :: state, performAction :: PerformAction eff state props action, render :: Render eff state props action, componentWillMount :: Maybe action, displayName :: Maybe String }
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
createClass :: forall eff state props action. Spec eff state props action -> ReactClass props
```

Create a component class from a `Spec`.


