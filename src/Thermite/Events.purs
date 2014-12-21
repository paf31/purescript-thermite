module Thermite.Events 
  ( onClick
  , onChange
  ) where

import Thermite
import Thermite.Html

foreign import onClick """
  function onClick(context) {
    return function(action) {
      return ["onClick", function(e) {
        context.performAction(action);
      }];
    };
  }
  """ :: forall action. Context action -> action -> Prop action

foreign import onChange """
  function onChange(context) {
    return function (f) {
      return ["onChange", function(e) {
        context.performAction(f(e.target.value));
      }];
    };
  }
  """ :: forall action. Context action -> (String -> action) -> Prop action

