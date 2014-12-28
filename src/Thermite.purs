module Thermite
  ( PerformAction()
  , Render()
  , Spec(..)
  , SpecRecord()
  , ComponentClass()
  , createClass 
  , render
  , component
  ) where

import DOM

import Control.Monad.Eff

import Thermite.Html
import Thermite.Action

type PerformAction state props action eff = props -> action -> Action eff state Unit 

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
        spec.performAction(self.props)(action)(self.state)(function(o) {
          return function() {
            self.setState(o.state);
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

foreign import component """
  function component(comp) {
    return function(props) {
      return function(children) {
        return React.createElement(comp, props, children);
      };
    };
  }
  """ :: forall props action eff. ComponentClass props eff -> props -> [Html action] -> Html action
