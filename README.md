## purescript-thermite

[![Pursuit](http://new-pursuit.purescript.org/packages/purescript-thermite/badge)](http://new-pursuit.purescript.org/packages/purescript-thermite/)

`purescript-thermite` is a simple PureScript wrapper for [`purescript-react`](http://github.com/purescript-contrib/purescript-react) inspired by `react-blaze`. It does not (and does not aim to) provide all of the functionality of ReactJS, but instead to provide a clean API to the most commonly-used parts of its API.

- [Module Documentation](docs/)
- [Example](test/Main.purs)
- [Task List Example](https://github.com/paf31/purescript-thermite-todomvc)

## Building

```
pulp dep update
pulp build
pulp test -r cat > html/index.js
```

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
import Prelude
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)
import Control.Monad.Eff
import qualified Thermite as T
import qualified Thermite.Action as T
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Types as DOM
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
render send s _ _ = R.div' [ counter, buttons ]
  where
  counter :: R.ReactElement
  counter =
    R.p'
      [ R.text "Value: "
      , R.text $ show s.counter
      ]

  buttons :: R.ReactElement 
  buttons =
    R.p'
      [ R.button [ RP.onClick \_ -> send Increment ]
                 [ R.text "Increment" ]
      , R.button [ RP.onClick \_ -> send Decrement ]
                 [ R.text "Decrement" ]
      ]
```

We interpret actions by using the `modifyState` function to update the component state:

```purescript
performAction :: T.PerformAction _ State _ Action
performAction _ Increment = T.modifyState \o -> { counter: o.counter + 1 }
performAction _ Decrement = T.modifyState \o -> { counter: o.counter - 1 }
```

The `Action` monad supports the following operations:

- `getState`, `setState`, `modifyState`
- `async`, `asyncSetState`
- `liftEff` via the `MonadEff` class

`async` and `asyncSetState` can be used to create asynchronous actions (using AJAX, for example). Each takes a callback, which will receive the result asynchronously. For example:

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
main =
  let component = T.createClass spec in
  body >>= R.render (R.createFactory component {}) 
  
  where
  body :: forall eff. Eff (dom :: DOM.DOM | eff) DOM.Element
  body = do
    win <- DOM.window
    doc <- DOM.document win
    elm <- fromJust <$> toMaybe <$> DOM.body doc
    return $ DOM.htmlElementToElement elm
```
