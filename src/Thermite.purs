module Thermite
  ( Action()
  , Spec(..)
  , SpecRecord()
  , ComponentClass()
  , createClass 
  , render
  ) where

import DOM

import Data.Maybe

import Control.Monad.Eff

import Thermite.Html

type Action eff a = (a -> Eff eff Unit) -> Eff eff Unit

newtype Spec eff state action = Spec (SpecRecord eff state action)

type SpecRecord eff state action =
  { initialState  :: state
  , setup         :: Maybe action
  , performAction :: state -> action -> Action eff state
  , render        :: Context action -> state -> Html action
  }

foreign import data ComponentClass :: # ! -> *

foreign import createClassImpl """
  function createClassImpl(spec) {
    return React.createClass({
      getInitialState: function() {
        return spec.initialState;
      },
      performAction: function(action) {
        var self = this;
        spec.performAction(self.state)(action)(function(state) {
          return function() {
            self.setState(state);
          };
        })();
      },
      render: function() {
        return spec.render(this)(this.state);
      }
    }); 
  }
  """ :: forall eff state action. SpecRecord eff state action -> ComponentClass eff

createClass :: forall eff state action. Spec eff state action -> ComponentClass eff
createClass (Spec spec) = createClassImpl spec

foreign import render """
  function render(component) {
    return function() {
      React.render(React.createElement(component, null), document.body);
    };
  }
  """ :: forall eff. ComponentClass eff -> Eff (dom :: DOM | eff) Unit

