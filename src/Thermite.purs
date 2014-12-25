module Thermite
  ( Action()
  , PerformAction()
  , Render()
  , Spec(..)
  , SpecRecord()
  , ComponentClass()
  , createClass 
  , render
  ) where

import DOM

import Control.Monad.Eff

import Thermite.Html

type Action eff a = (a -> Eff eff Unit) -> Eff eff Unit

type PerformAction state props action eff = state -> props -> action -> Action eff state

type Render state props action = Context action -> state -> props -> Html action

newtype Spec eff state props action = Spec (SpecRecord eff state props action)

type SpecRecord eff state props action =
  { initialState  :: state
  , performAction :: PerformAction state props action eff
  , render        :: Render state props action
  }

foreign import data ComponentClass :: * -> # ! -> *

foreign import createClassImpl """
  function createClassImpl(spec) {
    return React.createClass({
      getInitialState: function() {
        return spec.initialState;
      },
      performAction: function(action) {
        var self = this;
        spec.performAction(self.state)(self.props)(action)(function(state) {
          return function() {
            self.setState(state);
          };
        })();
      },
      render: function() {
        return spec.render(this)(this.state)(this.props);
      }
    }); 
  }
  """ :: forall eff state props action. SpecRecord eff state props action -> ComponentClass props eff

createClass :: forall eff state props action. Spec eff state props action -> ComponentClass props eff
createClass (Spec spec) = createClassImpl spec

foreign import render """
  function render(component) {
    return function(props) {
      return function() {
        React.render(React.createElement(component, props), document.body);
      };
    };
  }
  """ :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit

