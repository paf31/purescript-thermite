/* global exports */
"use strict";

// module Thermite.Internal

exports.getStateImpl = function(ctx) {
  return function() {
    return ctx.state.value;
  };
};

exports.setStateImpl = function(ctx) {
  return function(state) {
    return function() {
      ctx.setState({ value: state });
    };
  };
}

exports.textImpl = function(s) {
  return s;
}

exports.createElementImpl = function(element) {
  return function(props) {
    return function(children) {
      if ("dangerouslySetInnerHTML" in props) {
        return React.createElement(element, props);
      } else {
        return React.createElement(element, props, children);
      }
    };
  };
}

exports.unsafeAttribute = function(k) {
  return function(value) {
    var o = {};
    o[k] = value;
    return o;
  };
};

exports.event = function(name) {
  return function(context) {
    return function(f) {
      var o = {}; 
      o[name] = function(e) {
        context.performAction(f(e));
      };
      return o;
    };
  };
};

exports.documentBody = function() {
  return document.body;
};

exports.renderToImpl = function(element) {
  return function(component) {
    return function(props) {
      return function() {
        React.render(React.createElement(component, props), element);
      };
    };
  }
};