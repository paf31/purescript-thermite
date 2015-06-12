module Thermite.Internal where

import DOM

import Data.Maybe

import Control.Monad.Eff

import Thermite.Types

foreign import getStateImpl """
  function getStateImpl(ctx) {
    return function() {
      return ctx.state.value;
    };
  }
  """ :: forall eff state props action. Context state action -> Eff eff state

foreign import setStateImpl """
  function setStateImpl(ctx) {
    return function(state) {
      return function() {
        ctx.setState({ value: state });
      };
    };
  }
  """ :: forall eff state props action. Context state action -> state -> Eff eff Unit

foreign import textImpl """
  function textImpl(s) {
    return s;
  }
  """ :: forall eff. String -> Html eff

foreign import passArrayImpl """
  function passArrayImpl(a) {
    return a;
  }
  """ :: forall eff. [Html eff] -> Html eff

foreign import createElementImpl """
  function createElementImpl(element) {
    return function(props) {
      return function(children) {
        if ("dangerouslySetInnerHTML" in props) {
          return React.createElement(element, props);
        } else {
          return React.createElement.apply(this, [element, props].concat(children));
        }
      };
    };
  }
  """ :: forall element props eff. element -> props -> [Html eff] -> Html eff

foreign import unsafeAttribute """
  function unsafeAttribute(k) {
    return function(value) {
      var o = {};
      o[k] = value;
      return o;
    };
  }
  """ :: forall attr. String -> attr -> Attr

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
  """ :: forall state action event. String -> Context state action -> (event -> action) -> Attr

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
