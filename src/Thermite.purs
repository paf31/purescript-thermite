module Thermite
  ( createClass 
  , render
  ) where

import DOM

import Control.Monad.Eff

import Thermite.Html
import Thermite.Internal
import Thermite.Types
import Thermite.Action

createClass :: forall eff state props action. Spec (Action eff state) state props action -> ComponentClass props eff
createClass (Spec spec) = createClassImpl runAction spec

render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
render = renderImpl
