module Thermite.Html 
  ( Html()
  , Prop()
  , Props()
  , Context()
  , props
  , createElement  
  , text
  , onChange
  ) where

foreign import data Prop :: * -> *

foreign import data Props :: * -> *

foreign import data Context :: * -> *

foreign import props """
  function props(ps) {
    var result = {};

    for (var i = 0; i < ps.length; i++) {
      result[ps[i][0]] = ps[i][1];
    }

    return result;
  }
  """ :: forall action. [Prop action] -> Props action

foreign import data Html :: * -> * 

foreign import text """
  function text(s) {
    return s;
  }
  """ :: forall action. String -> Html action

foreign import createElement """
  function createElement(name) {
    return function(props) {
      return function(children) {
        return React.createElement(name, props, children);
      };
    };
  }
  """ :: forall action. String -> Props action -> [Html action] -> Html action

foreign import onChange """
  function onChange(context) {
    return function (f) {
      return ["onChange", function(e) {
        context.performAction(f(e.target.value));
      }];
    };
  }
  """ :: forall action. Context action -> (String -> action) -> Prop action

