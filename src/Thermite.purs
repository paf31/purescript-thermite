-- | Themite provides a simple model-view-action abstraction on top of `purescript-react`:
-- |
-- | - A `Spec` defines a `state` type which acts as the _model_.
-- | - The `Spec` also defines an `action` type which acts as the set of _actions_.
-- | - The `view` is a `Render` function which produces a React element for the current state.
-- | - The `PerformAction` function can be used to update the state based on an action.
-- |
-- | A `Spec` can be created using `simpleSpec`, and turned into a React component class using
-- | `createClass`.
-- |
-- | Thermite also provides type class instances and lens combinators for composing `Spec`s.

module Thermite
  ( PerformAction()
  , defaultPerformAction
  , EventHandler()
  , Render()
  , defaultRender
  , Spec()
  , _performAction
  , _render
  , simpleSpec
  , createClass
  , createReactSpec
  , withState
  , focus
  , focusState
  , match
  , foreach
  ) where

import Prelude

import DOM
import DOM.Node.Types

import Data.Lens
import Data.List
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Foldable (for_)

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe

-- | A type synonym for action handlers, which take an action, the current properties
-- | for the component, and a state update function, and return a computation in the `Eff` monad.
type PerformAction eff state props action =
  action ->
  props ->
  state ->
  (state -> Eff eff Unit) ->
  Eff eff Unit

-- | A default `PerformAction` action implementation which ignores all actions.
defaultPerformAction :: forall eff state props action. PerformAction eff state props action
defaultPerformAction _ _ _ _ = pure unit

-- | A type synonym for an event handler which can be used to construct
-- | `purescript-react`'s event attributes.
type EventHandler =
  forall eff refs.
    Eff ( props :: React.ReactProps
        , state :: React.ReactState React.ReadWrite
        , refs :: React.ReactRefs refs
        | eff
        ) Unit

-- | A rendering function, which takes an action handler function, the current state and
-- | props, an array of child nodes and returns a HTML document.
type Render state props action =
   (action -> EventHandler) ->
   props ->
   state ->
   Array React.ReactElement ->
   Array React.ReactElement

-- | A default `Render` implementation which renders nothing.
-- |
-- | This is useful when just `append`ing action handlers.
defaultRender :: forall state props action. Render state props action
defaultRender _ _ _ _ = []

-- | A component specification, which can be passed to `createClass`.
-- |
-- | A minimal `Spec` can be built using `simpleSpec`.
-- |
-- | The `Monoid` instance for `Spec` will compose `Spec`s by placing rendered
-- | HTML elements next to one another, and performing actions in sequence.
newtype Spec eff state props action = Spec
  { performAction      :: PerformAction eff state props action
  , render             :: Render state props action
  }

-- | A `Lens` for accessing the `PerformAction` portion of a `Spec`.
_performAction :: forall eff state props action. LensP (Spec eff state props action) (PerformAction eff state props action)
_performAction = lens (\(Spec s) -> s.performAction) (\(Spec s) pa -> Spec (s { performAction = pa }))

-- | A `Lens` for accessing the `Render` portion of a `Spec`.
-- |
-- | This can be useful when wrapping a `Render` function in order to frame a
-- | set of controls with some containing element. For example:
-- |
-- | ```purescript
-- | wrap :: Spec _ State _ Action -> Spec _ State _ Action
-- | wrap = over _render \child dispatch props state childre  ->
-- |   [ R.div [ RP.className "wrapper" ] [ child dispatch props state children ] ]
-- | ```
_render :: forall eff state props action. LensP (Spec eff state props action) (Render state props action)
_render = lens (\(Spec s) -> s.render) (\(Spec s) r -> Spec (s { render = r }))

-- | Create a minimal `Spec`. The arguments are, in order:
-- |
-- | - The `PerformAction` function for performing actions
-- | - The `Render` function for rendering the current state as a HTML document
-- |
-- | For example:
-- |
-- | ```purescript
-- | import qualified React.DOM as R
-- |
-- | data Action = Increment
-- |
-- | spec :: Spec _ Int _ Action
-- | spec = simpleSpec performAction render
-- |   where
-- |   render :: Render _ Int _
-- |   render _ _ n _ = [ R.text (show n) ]
-- |
-- |   performAction :: PerformAction _ Int _ Action
-- |   performAction Increment _ n k = k (n + 1)
-- | ```
simpleSpec ::
  forall eff state props action.
    PerformAction eff state props action ->
    Render state props action ->
    Spec eff state props action
simpleSpec performAction render =
  Spec { performAction: performAction
       , render: render
       }

instance semigroupSpec :: Semigroup (Spec eff state props action) where
  append (Spec spec1) (Spec spec2) =
    Spec { performAction:       \a p s k -> do spec1.performAction a p s k
                                               spec2.performAction a p s k
         , render:              \k p s   -> spec1.render k p s       <> spec2.render k p s
         }

instance monoidSpec :: Monoid (Spec eff state props action) where
  mempty = simpleSpec (\_ _ _ _ -> pure unit)
                      (\_ _ _ _ -> [])

-- | Create a React component class from a Thermite component `Spec`.
createClass :: forall eff state props action. Spec eff state props action -> state -> React.ReactClass props
createClass spec state = React.createClass <<< _.spec $ createReactSpec spec state

-- | Create a React component spec from a Thermite component `Spec`.
-- |
-- | This function is a low-level alternative to `createClass`, used when the React
-- | component spec needs to be modified before being turned into a component class,
-- | e.g. by adding additional lifecycle methods.
createReactSpec ::
  forall eff state props action.
  Spec eff state props action ->
  state ->
  { spec :: React.ReactSpec props state eff
  , dispatcher :: React.ReactThis props state -> action -> EventHandler
  }
createReactSpec (Spec spec) state =
  { spec: React.spec state render
  , dispatcher: dispatch
  }
  where
  dispatch :: React.ReactThis props state -> action -> EventHandler
  dispatch this action = do
    props <- React.getProps this
    state <- React.readState this
    unsafeInterleaveEff $ spec.performAction action props state (void <<< unsafeInterleaveEff <<< React.writeState this)

  render :: React.Render props state eff
  render this = map React.DOM.div' $
    spec.render (dispatch this)
      <$> React.getProps this
      <*> React.readState this
      <*> React.getChildren this

-- | This function captures the state of the `Spec` as a function argument.
-- |
-- | This can sometimes be useful in complex scenarios involving the `focus` and
-- | `foreach` combinators.
withState ::
  forall eff state props action.
  (state -> Spec eff state props action) ->
  Spec eff state props action
withState f = simpleSpec performAction render
  where
  performAction :: PerformAction eff state props action
  performAction a p st = view _performAction (f st) a p st

  render :: Render state props action
  render k p st = view _render (f st) k p st

-- | Change the state type, using a lens to focus on a part of the state.
-- |
-- | For example, to combine two `Spec`s, combining state types using `Tuple`
-- | and action types using `Either`:
-- |
-- | ```purescript
-- | spec1 :: Spec _ S1 _ A1
-- | spec2 :: Spec _ S2 _ A2
-- |
-- | spec :: Spec _ (Tuple S1 S2) _ (Either A1 A2)
-- | spec = focus _1 _Left spec1 <> focus _2 _Right spec2
-- | ```
-- |
-- | Actions will only be handled when the prism matches its input, otherwise
-- | the action will be ignored, and should be handled by some other component.
focus :: forall eff props state2 state1 action1 action2.
  LensP state2 state1 ->
  PrismP action2 action1 ->
  Spec eff state1 props action1 ->
  Spec eff state2 props action2
focus lens prism (Spec spec) = Spec
  { performAction: performAction
  , render: render
  }
  where
  performAction :: PerformAction eff state2 props action2
  performAction a p st k =
    case matching prism a of
      Left _ -> pure unit
      Right a' -> spec.performAction a' p (view lens st) (k <<< flip (set lens) st)

  render :: Render state2 props action2
  render k p st = spec.render (k <<< review prism) p (view lens st)

-- | A variant of `focus` which only changes the state type, by applying a `Lens`.
focusState :: forall eff props state2 state1 action.
  LensP state2 state1 ->
  Spec eff state1 props action ->
  Spec eff state2 props action
focusState lens = focus lens id

-- | A variant of `focus` which only changes the action type, by applying a `Prism`,
-- | effectively matching some subset of a larger action type.
match :: forall eff props state action1 action2.
  PrismP action2 action1 ->
  Spec eff state props action1 ->
  Spec eff state props action2
match prism = focus id prism

-- | Create a component whose state is described by a list, displaying one subcomponent
-- | for each entry in the list.
-- |
-- | The action type is modified to take the index of the originating subcomponent as an
-- | additional argument.
foreach :: forall eff props state action.
  (Int -> Spec eff state props action) ->
  Spec eff (List state) props (Tuple Int action)
foreach f = Spec
  { performAction: performAction
  , render: render
  }
  where
  performAction :: PerformAction eff (List state) props (Tuple Int action)
  performAction (Tuple i a) p sts k =
    for_ (sts !! i) \st -> case f i of Spec s -> s.performAction a p st (k <<< modifying i)
    where
    modifying :: Int -> state -> List state
    modifying i st = fromMaybe sts (updateAt i st sts)

  render :: Render (List state) props (Tuple Int action)
  render k p sts _ = foldWithIndex (\i st els -> case f i of Spec s -> els <> s.render (k <<< Tuple i) p st []) sts []

  foldWithIndex :: forall a r. (Int -> a -> r -> r) -> List a -> r -> r
  foldWithIndex f = go 0
    where
    go _ Nil         r = r
    go i (Cons x xs) r = go (i + 1) xs (f i x r)
