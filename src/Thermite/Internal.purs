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
    return function(ps) {
      return function(children) {
        var props = {};

        for (var i = 0; i < ps.length; i++) {
          var p = ps[i];
          props[p[0]] = p[1];
        }

        return React.createElement(name, props, children);
      };
    };
  }
  """ :: forall action. String -> Props action -> [Html action] -> Html action

foreign import createElementFromClass """
  function createElementFromClass(clazz) {
    return function(ps) {
      return function(children) {
        var props = {};
        var n = ps.length;
        var i = -1;

        while (++i < n) {
          var p = ps[i];
          props[p[0]] = p[1];
        }

        return React.createElement(clazz, props, children);
      };
    };
  }
  """ :: forall eff props action. ComponentClass props eff -> Props action -> [Html action] -> Html action

foreign import unsafeAttribute """
  function unsafeAttribute(attr) {
    return function(value) {
      return [attr, value];
    };
  }
  """ :: forall action attr. String -> attr -> Prop action

foreign import event """
  function event(name) {
    return function(context) {
      return function(f) {
        return [name, function(e) {
          context.performAction(f(e));
        }];
      };
    };
  }
  """ :: forall state props action event. String -> Context state props action -> (event -> action) -> Prop action

foreign import createClassImpl """
  function createClassImpl(runAction) {
    return function(maybe) {
      return function(spec) {
        return React.createClass({
          getInitialState: function() {
            return spec.initialState;
          },
          performAction: function(action) {
            runAction(this)(spec.performAction(this)(action))();
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

foreign import renderImpl """
  function renderImpl(component) {
    return function(props) {
      return function() {
        React.render(React.createElement(component, props), document.body);
      };
    };
  }
  """ :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit

