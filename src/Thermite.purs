-- | Thermite provides a simple model-view-action abstraction on top of `purescript-react`:
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
  ( PerformAction
  , defaultPerformAction
  , Dispatch
  , Render
  , defaultRender
  , writeState
  , modifyState
  , Spec
  , _performAction
  , _render
  , simpleSpec
  , createClass
  , createReactConstructor
  , defaultMain
  , withState
  , focus
  , focusState
  , match
  , split
  , foreach
  , cmapProps

  , module T
  ) where

import Prelude
import Effect.Aff (Aff, launchAff, makeAff, nonCanceler, Fiber)
import Effect (Effect)
import Control.Monad.Free.Trans (resume)
import Control.Monad.Rec.Class (Step(..), forever, tailRecM)
import Control.Coroutine (CoTransform(CoTransform), CoTransformer, cotransform, transform, transformCoTransformL, transformCoTransformR)
import Control.Coroutine (CoTransformer, cotransform) as T
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (body) as Web
import Web.HTML.HTMLElement (toElement) as Web
import Web.HTML.Window (document) as Web
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens (Prism', Lens', matching, view, review, preview, lens, over)
import Data.List (List(..), (!!), modifyAt)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Tuple (Tuple(..))
import React (unsafeCreateElement, Children)
import React as React
import ReactDOM (render)

-- | A type synonym for an action handler, which takes an action, the current props
-- | and state for the component, and return a `CoTransformer` which will emit
-- | state updates asynchronously.
-- |
-- | `Control.Coroutine.cotransform` can be used to emit state update functions
-- | and wait for the new state value. If `cotransform` returns `Nothing`, then
-- | the state could not be updated. Usually, this will not happen, but it is possible
-- | in certain use cases involving `split` and `foreach`.
type PerformAction state props action
   = action
  -> props
  -> state
  -> CoTransformer (Maybe state) (state -> state) Aff Unit

-- | A default `PerformAction` action implementation which ignores all actions.
defaultPerformAction :: forall state props action. PerformAction state props action
defaultPerformAction _ _ _ = pure unit

-- | Replace the current component state.
writeState :: forall state. state -> CoTransformer (Maybe state) (state -> state) Aff (Maybe state)
writeState st = cotransform (const st)

-- | An alias for `cotransform` - apply a function to the current component state.
modifyState :: forall state. (state -> state) -> CoTransformer (Maybe state) (state -> state) Aff (Maybe state)
modifyState = cotransform

-- | A type synonym for a `dispatch`able function of action types.
type Dispatch action = action -> Effect (Fiber Unit)

-- | A rendering function, which takes an action handler function, the current state and
-- | props, an array of child nodes and returns a HTML document.
type Render state props action
   = Dispatch action
  -> props
  -> state
  -> Array React.ReactElement
  -> Array React.ReactElement

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
newtype Spec state props action = Spec
  { performAction      :: PerformAction state props action
  , render             :: Render state props action
  }

cmapProps
  :: forall state props props' action
   . (props' -> props)
  -> Spec state props action
  -> Spec state props' action
cmapProps f (Spec sp) = Spec { performAction, render }
  where
    performAction a = sp.performAction a <<< f
    render a = sp.render a <<< f

-- | A `Lens` for accessing the `PerformAction` portion of a `Spec`.
_performAction :: forall state props action. Lens' (Spec state props action) (PerformAction state props action)
_performAction = lens (\(Spec s) -> s.performAction) (\(Spec s) pa -> Spec (s { performAction = pa }))

-- | A `Lens` for accessing the `Render` portion of a `Spec`.
-- |
-- | This can be useful when wrapping a `Render` function in order to frame a
-- | set of controls with some containing element. For example:
-- |
-- | ```purescript
-- | wrap :: Spec State _ Action -> Spec State _ Action
-- | wrap = over _render \child dispatch props state childre  ->
-- |   [ R.div [ RP.className "wrapper" ] [ child dispatch props state children ] ]
-- | ```
_render :: forall state props action. Lens' (Spec state props action) (Render state props action)
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
-- | spec :: Spec Int _ Action
-- | spec = simpleSpec performAction render
-- |   where
-- |   render :: Render _ Int _
-- |   render _ _ n _ = [ R.text (show n) ]
-- |
-- |   performAction :: PerformAction Int _ Action
-- |   performAction Increment _ n k = k (n + 1)
-- | ```
simpleSpec
  :: forall state props action
   . PerformAction state props action
  -> Render state props action
  -> Spec state props action
simpleSpec performAction render =
  Spec { performAction: performAction
       , render: render
       }

instance semigroupSpec :: Semigroup (Spec state props action) where
  append (Spec spec1) (Spec spec2) =
    Spec { performAction:       \a p s -> do spec1.performAction a p s
                                             spec2.performAction a p s
         , render:              \k p s   -> spec1.render k p s <> spec2.render k p s
         }

instance monoidSpec :: Monoid (Spec state props action) where
  mempty = simpleSpec defaultPerformAction defaultRender

-- | Create a React component class from a Thermite component `Spec`.
createClass
  :: forall state props action
   . Spec { | state } { children :: Children | props } action
  -> { | state } -- ^ Initial State
  -> String -- ^ Component Name
  -> React.ReactClass { children :: Children | props }
createClass spec state name = React.component name (createReactConstructor spec state).constructor

-- | Create a React component constructor from a Thermite component `Spec` with an additional
-- | function for converting the rendered Array of ReactElement's into a single ReactElement
-- | as is required by React.
-- |
-- | This function is a low-level alternative to `createClass`, used when the React
-- | component constructor needs to be modified before being turned into a component class,
-- | e.g. by adding additional lifecycle methods.
createReactConstructor
  :: forall state props action
   . Spec { | state } { children :: Children | props } action
  -> { | state } -- ^ Initial State
  -> { constructor :: React.ReactClassConstructor { children :: Children | props } { | state } (render :: React.Render, state :: { | state })
     , dispatcher :: React.ReactThis { children :: Children | props } { | state } -> Dispatch action
     }
createReactConstructor (Spec spec) initState =
  { constructor: \this -> pure { render: render this, state: initState }
  , dispatcher
  }
  where
    dispatcher :: React.ReactThis { children :: Children | props } { | state } -> Dispatch action
    dispatcher this action = do
      let step :: CoTransformer (Maybe { | state }) ({ | state } -> { | state }) Aff Unit
               -> Aff (Step (CoTransformer (Maybe { | state }) ({ | state } -> { | state }) Aff Unit) Unit)
          step cot = do
            e <- resume cot
            case e of
              Left _ -> pure (Done unit)
              Right (CoTransform f k) -> do
                -- cotransform state stored in the React component, then apply the new state while in lock-step with
                -- React's ability to digest state mutations.
                newState <- makeAff \resolve -> do
                  st <- React.getState this
                  let st' = f st
                  nonCanceler <$ React.writeStateWithCallback this st' (resolve (Right st'))
                pure (Loop (k (Just newState)))

      props <- React.getProps this
      state <- React.getState this
      let cotransformer :: CoTransformer (Maybe { | state }) ({ | state } -> { | state }) Aff Unit
          cotransformer = spec.performAction action props state

      -- Step the coroutine manually, since none of the existing coroutine
      -- functions do quite what we want here.
      launchAff (tailRecM step cotransformer)

    render :: React.ReactThis { children :: Children | props } { | state } -> React.Render
    render this = do
      props <- React.getProps this
      state <- React.getState this
      pure $ React.toElement $ spec.render (dispatcher this) props state (React.childrenToArray props.children)

-- | A default implementation of `main` which renders a component to the
-- | document body.
defaultMain
  :: forall state props action
   . Spec { | state } { children :: Children | props } action
  -> { | state } -- ^ Initial State
  -> String -- ^ Component Name
  -> { | props } -- ^ Read-Only Props
  -> Effect Unit
defaultMain spec initialState name props = void do
  let component :: React.ReactClass { children :: Children | props }
      component = createClass spec initialState name
  container <- Web.body =<< Web.document =<< Web.window
  traverse_ (render (unsafeCreateElement component props [] {- (React.childrenToArray props.children) -}) <<< Web.toElement) container

-- | This function captures the state of the `Spec` as a function argument.
-- |
-- | This can sometimes be useful in complex scenarios involving the `focus` and
-- | `foreach` combinators.
withState
  :: forall state props action
   . (state -> Spec state props action)
  -> Spec state props action
withState f = simpleSpec performAction render
  where
    performAction :: PerformAction state props action
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
focus
  :: forall props state2 state1 action1 action2
   . Lens' state2 state1
  -> Prism' action2 action1
  -> Spec state1 props action1
  -> Spec state2 props action2
focus lens prism (Spec spec) = Spec { performAction, render }
  where
    performAction :: PerformAction state2 props action2
    performAction a p st =
      case matching prism a of
        Left _ -> pure unit
        Right a' -> forever (transform (map (view lens)))
                    `transformCoTransformL` spec.performAction a' p (view lens st)
                    `transformCoTransformR` forever (transform (over lens))

    render :: Render state2 props action2
    render k p st = spec.render (k <<< review prism) p (view lens st)

-- | A variant of `focus` which only changes the state type, by applying a `Lens`.
focusState
  :: forall props state2 state1 action
   . Lens' state2 state1
  -> Spec state1 props action
  -> Spec state2 props action
focusState lens = focus lens identity

-- | A variant of `focus` which only changes the action type, by applying a `Prism`,
-- | effectively matching some subset of a larger action type.
match
  :: forall props state action1 action2
   . Prism' action2 action1
  -> Spec state props action1
  -> Spec state props action2
match prism = focus identity prism

-- | Create a component which renders an optional subcomponent.
split
  :: forall props state1 state2 action
   . Prism' state1 state2
  -> Spec state2 props action
  -> Spec state1 props action
split prism (Spec spec) = Spec { performAction, render }
  where
    performAction :: PerformAction state1 props action
    performAction a p st =
      case matching prism st of
        Left _ -> pure unit
        Right st2 -> forever (transform (_ >>= preview prism))
                     `transformCoTransformL` spec.performAction a p st2
                     `transformCoTransformR` forever (transform (over prism))

    render :: Render state1 props action
    render k p st children =
      case matching prism st of
        Left _ -> []
        Right st' -> spec.render k p st' children

-- | Create a component whose state is described by a list, displaying one subcomponent
-- | for each entry in the list.
-- |
-- | The action type is modified to take the index of the originating subcomponent as an
-- | additional argument.
foreach
  :: forall props state action
   . (Int -> Spec state props action)
  -> Spec (List state) props (Tuple Int action)
foreach f = Spec
    { performAction: performAction
    , render: render
    }
  where
    performAction :: PerformAction (List state) props (Tuple Int action)
    performAction (Tuple i a) p sts =
        for_ (sts !! i) \st ->
          case f i of
            Spec s -> forever (transform (_ >>= (_ !! i)))
                      `transformCoTransformL` s.performAction a p st
                      `transformCoTransformR` forever (transform (modifying i))
      where
        modifying :: Int -> (state -> state) -> List state -> List state
        modifying j g sts' = fromMaybe sts' (modifyAt j g sts')

    render :: Render (List state) props (Tuple Int action)
    render k p sts _ = foldWithIndex (\i st els -> case f i of Spec s -> els <> s.render (k <<< Tuple i) p st []) sts []

    foldWithIndex :: forall a r. (Int -> a -> r -> r) -> List a -> r -> r
    foldWithIndex g = go 0
      where
      go _ Nil         r = r
      go i (Cons x xs) r = go (i + 1) xs (g i x r)
