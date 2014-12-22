module Thermite.Html 
  ( Html()
  , Prop()
  , Props()
  , Context()
  , createElement  
  , text
  ) where

foreign import data Prop :: * -> *

foreign import data Context :: * -> *

type Props action = [Prop action]

foreign import data Html :: * -> * 

foreign import text """
  function text(s) {
    return s;
  }
  """ :: forall action. String -> Html action

foreign import createElement """
  function createElement(name) {
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
