module Thermite
  ( simpleSpec
  , componentWillMount
  , createClass
  , displayName
  , render
  ) where

import DOM

import Data.Maybe

import Control.Monad.Eff

import Thermite.Html
import Thermite.Internal
import Thermite.Types
import Thermite.Action

simpleSpec :: forall m state props action. state -> PerformAction props action m -> Render state props action -> Spec m state props action
simpleSpec initialState performAction render = Spec { initialState: initialState
                                                    , performAction: performAction
                                                    , render: render
                                                    , componentWillMount: Nothing
                                                    , displayName: Nothing
                                                    }

componentWillMount :: forall m state props action. action -> Spec m state props action -> Spec m state props action
componentWillMount action (Spec spec) = Spec (spec { componentWillMount = Just action })

displayName :: forall m state props action. String -> Spec m state props action -> Spec m state props action
displayName name (Spec spec) = Spec (spec { displayName = Just name })

createClass :: forall eff state props action. Spec (Action eff state) state props action -> ComponentClass props eff
createClass = createClassImpl runAction maybe

render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
render = renderImpl
