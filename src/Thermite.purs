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
  , render
  , renderTo
  ) where

import DOM

import Data.Maybe

import Control.Monad.Eff

import Thermite.Html
import Thermite.Internal
import Thermite.Types
import Thermite.Action

-- | A type synonym for action handlers, which take an action and the current properties
-- | for the component, and return a computation in the `Action` monad.
type PerformAction eff state props action = props -> action -> Action eff state Unit

-- | A rendering function, which takes a `Context`, the current state and properties, an array
-- | of child nodes and returns a HTML document.
type Render eff state props action = Context state action -> state -> props -> [Html eff] -> Html eff

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

foreign import createClassImpl 
  "function createClassImpl(runAction) {\
  \  return function(maybe) {\
  \    return function(spec) {\
  \      return React.createClass({\
  \        getInitialState: function() {\
  \          return {\
  \            value: spec.initialState\
  \          };\
  \        },\
  \        performAction: function(action) {\
  \          runAction(this)(spec.performAction(this.props)(action))();\
  \        },\
  \        render: function() {\
  \          var children = Array.isArray(this.props.children) ? this.props.children : [this.props.children];\
  \          return spec.render(this)(this.state.value)(this.props)(children);\
  \        },\
  \        componentWillMount: function() {\
  \          var self = this;\
  \          maybe(function() { })(function(action) {\
  \            return function() {\
  \              self.performAction(action);\
  \            };\
  \          })(spec.componentWillMount)();\
  \        },\
  \        displayName: maybe(undefined)(function(a) {\
  \          return a;\
  \        })(spec.displayName)\
  \      })\
  \    };\
  \  };\
  \}" :: forall eff state props action. (Context state action -> Action eff state Unit -> Eff eff Unit) ->
                                        (forall a r. r -> (a -> r) -> Maybe a -> r) ->
                                        Spec eff state props action ->
                                        ComponentClass props eff

-- | Create a component class from a `Spec`.
createClass :: forall eff state props action. Spec eff state props action -> ComponentClass props eff
createClass = createClassImpl runAction maybe

-- | Render a component class to the document body.
render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
render cc props = do
  body <- documentBody
  renderTo body cc props

-- | Render a component class to the specified node.
renderTo :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
renderTo = renderToImpl
