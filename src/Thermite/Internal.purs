module Thermite.Internal where

import DOM

import Data.Maybe

import Control.Monad.Eff

import Thermite.Types

foreign import getStateImpl """
  function getStateImpl(ctx) {
    return function() {
      return ctx.state;
    };
  }
  """ :: forall eff state props action. Context state props action -> Eff eff state

foreign import setStateImpl """
  function setStateImpl(ctx) {
    return function(state) {
      return function() {
        ctx.setState(state);
      };
    };
  }
  """ :: forall eff state props action. Context state props action -> state -> Eff eff Unit

foreign import textImpl """
  function textImpl(s) {
    return s;
  }
  """ :: forall action. String -> Html action

foreign import createElementImpl """
  function createElementImpl(name) {
    return function(attr) {
      return function(children) {
        return React.createElement(name, attr, children);
      };
    };
  }
  """ :: forall action. String -> Attr action -> [Html action] -> Html action

foreign import unsafeAttribute """
  function unsafeAttribute(attr) {
    return function(value) {
      var o = {};
      o.attr = value;
      return o;
    };
  }
  """ :: forall action attr. String -> attr -> Attr action

foreign import event """
  function event(name) {
    return function(context) {
      return function(f) {
        var o = {}; 
        o[name] = function(e) {
          context.performAction(f(e));
        };
        return o;
      };
    };
  }
  """ :: forall state props action event. String -> Context state props action -> (event -> action) -> Attr action

foreign import createClassImpl """
  function createClassImpl(runAction) {
    return function(maybe) {
      return function(spec) {
        return React.createClass({
          getInitialState: function() {
            return spec.initialState;
          },
          performAction: function(action) {
            runAction(this)(spec.performAction(this.props)(action))();
          },
          render: function() {
            return spec.render(this)(this.state)(this.props);
          },
          componentWillMount: function() {
            var self = this;
            maybe(function() { })(function(action) {
              return function() {
                self.performAction(action);
              };
            })(spec.componentWillMount)();
          },
          displayName: maybe(undefined)(function(a) { return a; })(spec.displayName)
        })
      };
    };
  }
  """ :: forall eff m state props action. (Context state props action -> m Unit -> Eff eff Unit) ->
                                          (forall a r. r -> (a -> r) -> Maybe a -> r) ->
                                          Spec m state props action ->
                                          ComponentClass props eff

foreign import documentBody """
  function documentBody(component) {
    return document.body;
  }
  """ :: forall props eff. Eff (dom :: DOM | eff) Node

foreign import renderToImpl """
  function renderToImpl(element) {
    return function (component) {
      return function(props) {
        return function() {
          React.render(React.createElement(component, props), element);
        };
      };
    }
  }
  """ :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
