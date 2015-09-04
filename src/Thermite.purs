-- | This module defines functions for working with React components at a high level:
-- |
-- | - `createClass` and `simpleSpec`, which can be used to create a component class
-- | - `render` and `renderTo`, which can be used to render a component class

module Thermite
  ( PerformAction()
  , Render()
  , Spec()
  , SpecRecord()
  , simpleSpec
  , componentWillMount
  , createClass
  , displayName
  ) where

import Prelude

import DOM
import DOM.Node.Types

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe

import Thermite.Action

import Unsafe.Coerce

-- | A type synonym for action handlers, which take an action and the current properties
-- | for the component, and return a computation in the `Action` monad.
type PerformAction eff state props action = props -> action -> Action eff state Unit

-- | A rendering function, which takes a event dispatcher function, the current state and
-- | props, an arrayof child nodes and returns a HTML document.
type Render eff state props action = 
     (action -> React.EventHandlerContext eff props state Unit) -> 
     state -> 
     props -> 
     Array React.ReactElement -> 
     React.ReactElement

-- | A component specification, which can be passed to `createClass`.
-- |
-- | A minimal `Spec` can be built using `simpleSpec`, and extended with optional arguments
-- | using functions in the `Thermite` module.
newtype Spec eff state props action = Spec (SpecRecord eff state props action)

-- | A type synonym for the record type which captures the functions which make up a `Spec`.
type SpecRecord eff state props action =
  { initialState       :: state
  , performAction      :: PerformAction eff state props action
  , render             :: Render eff state props action
  , componentWillMount :: Maybe action
  , displayName        :: Maybe String
  }

-- | Create a minimal `Spec`. The arguments are, in order:
-- |
-- | - The initial component state
-- | - A function for performing actions
-- | - A function for rendering the current state as a HTML document
-- |
-- | A `Spec` created using this function can be extended with optional properties using other functions
-- | in this module.
simpleSpec :: forall eff state props action.
                state ->
                PerformAction eff state props action ->
                Render eff state props action ->
                Spec eff state props action
simpleSpec initialState performAction render = Spec { initialState: initialState
                                                    , performAction: performAction
                                                    , render: render
                                                    , componentWillMount: Nothing
                                                    , displayName: Nothing
                                                    }

-- | Extend a `Spec` with an action to run when the component will be mounted.
componentWillMount :: forall eff state props action. action -> Spec eff state props action -> Spec eff state props action
componentWillMount action (Spec spec) = Spec (spec { componentWillMount = Just action })

-- | Extend a `Spec` with a display name.
displayName :: forall eff state props action. String -> Spec eff state props action -> Spec eff state props action
displayName name (Spec spec) = Spec (spec { displayName = Just name })

-- | Create a component class from a `Spec`.
createClass :: forall eff state props action. Spec eff state props action -> React.ReactClass props
createClass (Spec spec) = React.createClass $ (React.spec spec.initialState render) 
                                                { displayName = displayName_
                                                , componentWillMount = componentWillMount_
                                                }
  where
  displayName_ :: String
  displayName_ = fromMaybe "" spec.displayName
  
  componentWillMount_ :: forall eff. React.ReactThis props state -> Eff ( props :: React.ReactProps props
                                                                        , state :: React.ReactState (React.Read React.Write) state
                                                                        , refs :: React.ReactRefs React.Disallowed | eff
                                                                        ) Unit
  componentWillMount_ ctx = maybe (return unit) (unsafeInterleaveEff <<< dispatch ctx) spec.componentWillMount
      
  unsafeInterleaveAction :: forall eff1 eff2 state a. Action eff1 state a -> Action eff2 state a
  unsafeInterleaveAction = unsafeCoerce
      
  dispatch :: React.ReactThis props state -> action -> React.EventHandlerContext eff props state Unit
  dispatch this action = do
    props <- React.getProps this
    runAction this $ unsafeInterleaveAction $ spec.performAction props action
      
  render :: React.Render props state eff
  render this = spec.render (dispatch this)
                  <$> React.readState this 
                  <*> React.getProps this 
                  <*> React.getChildren this
