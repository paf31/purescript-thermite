## purescript-thermite

[![Pursuit](http://pursuit.purescript.org/packages/purescript-thermite/badge)](http://pursuit.purescript.org/packages/purescript-thermite/)

`purescript-thermite` is a simple PureScript wrapper for [`purescript-react`](http://github.com/purescript-contrib/purescript-react) inspired by `react-blaze`. It does not (and does not aim to) provide all of the functionality of ReactJS, but instead to provide a clean API to the most commonly-used parts of its API.

- [Try Thermite!](http://paf31.github.io/try-thermite)
- [Module Documentation](docs/)
- Example Project: [Code](test/), [Demo](http://functorial.com/purescript-thermite-todomvc/)

## Building

```
pulp dep update
pulp build
pulp test -r cat > html/index.js
```

You can also now use `npm test` to run the test command above.

## Getting Started

Thermite components are defined in parts:

- A type of _actions_, which represents the actions a user can take on our component
- A type of _states_, which represents the internal state of our component
- An initial state
- A rendering function, which takes the current component state and properties, and creates a HTML document
- A function which interprets _actions_, by modifying the state and/or running some (possibly asynchronous) computations

Here is an example. We'll build a component which displays the value of a integer-valued counter.

First of all, we need to import some modules:

```purescript
import qualified Thermite as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
```

In our component, users will be able to take two actions - increment and decrement - which will be represented as buttons later:

```purescript
data Action = Increment | Decrement
```

The state of our component is just an integer:

```purescript
type State = { counter :: Int }
```

The initial state is zero:

```purescript
initialState :: State
initialState = { counter: 0 }
```

Our rendering function uses the `React.DOM.*` modules to create a HTML document containing a label and two buttons. The buttons' `onclick` handlers are given functions which generate the correct actions. The `dispatch` function, which is passed as the first argument to `render`, can be used to build such a function, by providing an action:

```purescript
render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text "Value: "
         , R.text $ show state.counter
         ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch Increment ]
                    [ R.text "Increment" ]
         , R.button [ RP.onClick \_ -> dispatch Decrement ]
                    [ R.text "Decrement" ]
         ]
  ]
```

The `performAction` function interprets actions by passing a function to the state update function, which is responsible for updating the state using record updates:

```purescript
performAction :: T.PerformAction _ State _ Action
performAction Increment _ _ update = update $ \state -> state { counter = state.counter + 1 }
performAction Decrement _ _ update = update $ \state -> state { counter = state.counter - 1 }
```

_Note_: since `PerformAction` is callback-based, we can also create asynchronous action handlers (using AJAX, for example):

```purescript
performAction :: T.PerformAction _ State _ Action
performAction Increment _ _ update = getIncrementValueFromServer \amount ->
  update $ \state -> state { counter = state.counter + amount }
```

With these pieces, we can create a `Spec` for our component:

```purescript
spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
```

Finally, in `main`, we use `createClass` to turn our `Spec` into a component class, providing an initial state.
The `render` function from `purescript-react` can then be used to render our component to the document body:

```purescript
main = do
  let component = T.createClass spec initialState
  body >>= R.render (R.createFactory component {})
```

## Combining Components

The `Spec` type is an instance of the `Semigroup` and `Monoid` type classes. These instances can be used to combine different components with the same `state` and `action` types.

In practice, the `state` and `action` types will not always match for the different subcomponents, so Thermite provides combinators for changing these type arguments: `focus` and `foreach`. These combinators are heavily inspired by the [OpticUI](https://github.com/zrho/purescript-optic-ui) library.

See the [example project](test/Main.purs) for examples of these kinds of composition.

### `focus`

`focus` (and the related functions `focusState` and `match`) are used to enlarge the state and action types, to make it possible to embed a component inside a larger component.

`focus` takes a _lens_, which identifies the `state` type as a part of the state type of the larger component, and a _prism_, which identifies all actions of the smaller component as actions for the larger component. `focusState` is used when only the `state` type needs to be changed, and `match` is used when only the `action` type needs to be changed.

As a simple example, we can combine two subcomponents by using a `Tuple` to store both states, and `Either` to combine both sets of actions:

```purescript
spec1 :: Spec _ S1 _ A1
spec2 :: Spec _ S2 _ A2

spec :: Spec _ (Tuple S1 S2) _ (Either A1 A2)
spec = focus _1 _Left spec1 <> focus _2 _Right spec2
```

Here, `_1` and `_Left` embed `spec1` inside `spec`, using the left components of both the state `Tuple` and the `Either` type of actions. `_2` and `_Right` similarly embed `spec2`, using the right components.

_focus_ is responsible for directing the various actions to the correct components, and updating the correct parts of the state.

### `split`

`split` is used to handle child components which might not be present, for
example, when a parent object contains a `Maybe` state.

```purescript
type Parent = { child :: Maybe child }

_child :: LensP Parent (Maybe Child)
_child = lens _.child (_ { child = _ })

_ChildAction :: PrismP ParentAction ChildAction

childSpec :: Spec _ Child _ ChildAction

spec :: Spec _ Parent _ ParentAction
spec = focus _child _ChildAction $ split _Just childSpec
```

### `foreach`

Where `focus` embeds a single subcomponent inside another component, `foreach` embeds a whole collection of subcomponents.

`foreach` turns a `Spec eff state props action` into a `Spec eff (List state) props (Tuple Int action)`. Note that the state type has been wrapped using `List`, since the component now tracks state for each element of the collection. Also, the `action` type has been replaced with `Tuple Int action`. This means that when an action occurs, it is accompanied by the index of the element in the collection which it originated from.
