## purescript-thermite

[![Pursuit](http://new-pursuit.purescript.org/packages/purescript-thermite/badge)](http://new-pursuit.purescript.org/packages/purescript-thermite/)

`purescript-thermite` is a simple PureScript wrapper for ReactJS inspired by `react-blaze`. It does not (and does not aim to) provide all of the functionality of ReactJS, but instead to provide a clean API to the most commonly-used parts of its API.

- [Module Documentation](docs/)
- [Example](test/Main.purs)
- [Task List Example](https://github.com/paf31/purescript-thermite-todomvc)

## Building

```
npm install
bower update
grunt
```

## Getting Started

Thermite components are defined in parts:

- A type of _actions_, which represents the actions a user can take on our component
- A type of _states_, which represents the internal state of our component
- An initial state
- A rendering function, which takes the current component state and properties, and creates a HTML document
- A function which interprets _actions_, by modifying the state and/or running some (possibly asynchronous) computations

Here is an example. We'll build a component which displays the value of a integer-valued counter.

First of all, we need to import various modules:

```purescript
import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T
```

In our component, users will be able to take two actions - increment and decrement - which will be represented as buttons later:

```purescript
data Action = Increment | Decrement
```

The state of our component is just a `Number`:

```purescript
type State = { counter :: Int }
```

The initial state is zero:

```purescript
initialState :: State
initialState = { counter: 0 }
```

Our rendering function uses the `Thermite.Html.*` modules to create a HTML document containing a label and two buttons. The buttons' `onclick` handlers are given functions which generate the correct actions. We also pass the _context_ `ctx` to the `onclick` handlers, so that the event handlers are able to update the state of the component.

```purescript
render :: T.Render _ State _ Action
render ctx s _ _ = T.div' [counter, buttons]
  where
  counter :: T.Html _
  counter =
    T.p'
      [ T.text "Value: "
      , T.text $ show s.counter
      ]

  buttons :: T.Html _
  buttons =
    T.p'
      [ T.button (T.onClick ctx (\_ -> Increment))
                 [ T.text "Increment" ]
      , T.button (T.onClick ctx (\_ -> Decrement))
                 [ T.text "Decrement" ]
      ]
```

We interpret actions by using the `modifyState` function to update the component state:

```purescript
performAction :: T.PerformAction _ State _ Action
performAction _ Increment = T.modifyState \o -> { counter: o.counter + 1 }
performAction _ Decrement = T.modifyState \o -> { counter: o.counter - 1 }
```

With these pieces, we can create a specification for our component:

```purescript
spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
```

Finally, in `main`, we use `createClass` to turn our `Spec` into a component class, and `render` to render it to the document body:

```purescript
main = do
  let component = T.createClass spec
  T.render component {}
```
