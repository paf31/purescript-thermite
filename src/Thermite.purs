-- | This module defines functions for working with React components at a high level:
-- |
-- | - `createClass` and `simpleSpec`, which can be used to create a component class
-- | - `render` and `renderTo`, which can be used to render a component class

module Thermite
  ( simpleSpec
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

-- | Create a minimal `Spec`. The arguments are, in order:
-- |
-- | - The initial component state
-- | - A function for performing actions
-- | - A function for rendering the current state as a HTML document
-- |
-- | A `Spec` created using this function can be extended with optional properties using other functions
-- | in this module.
simpleSpec :: forall m state props action. state -> PerformAction props action m -> Render state props action -> Spec m state props action
simpleSpec initialState performAction render = Spec { initialState: initialState
                                                    , performAction: performAction
                                                    , render: render
                                                    , componentWillMount: Nothing
                                                    , displayName: Nothing
                                                    }

-- | Extend a `Spec` with an action to run when the component will be mounted.
componentWillMount :: forall m state props action. action -> Spec m state props action -> Spec m state props action
componentWillMount action (Spec spec) = Spec (spec { componentWillMount = Just action })

-- | Extend a `Spec` with a display name.
displayName :: forall m state props action. String -> Spec m state props action -> Spec m state props action
displayName name (Spec spec) = Spec (spec { displayName = Just name })

-- | Create a component class from a `Spec`.
createClass :: forall eff state props action. Spec (Action eff state) state props action -> ComponentClass props eff
createClass = createClassImpl runAction maybe

-- | Render a component class to the document body.
render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
render cc props = do
  body <- documentBody
  renderTo body cc props

-- | Render a component class to the specified node.
renderTo :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
renderTo = renderToImpl
