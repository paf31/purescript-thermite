-- | This module defines types used by the Thermite library.

module Thermite.Types 
  ( Context()
  , ComponentClass()
  , Attr()
  , Html()
  ) where

import Data.Maybe
import Data.Monoid

-- | The `Context` type represents React's `this` reference.
-- |
-- | It is passed to event handlers to allow us to get and set component state.
data Context state action

-- | A component class, the result of React's `createClass` method.
-- |
-- | The type parameters capture the properties required by the class, and the effects
-- | its action handlers can have.
data ComponentClass props (eff :: # !)

-- | The type of HTML attributes.
data Attr

-- | The type of HTML elements.
data Html (eff :: # !)
  
foreign import emptyAttr """
  var emptyAttr = {};
  """ :: Attr

foreign import appendAttr """
  function appendAttr(attr1) {
    return function(attr2) {
      var o = {};
      for (var k in attr1) {
        if (attr1.hasOwnProperty(k)) {
          o[k] = attr1[k];
        }
      }
      for (var k in attr2) {
        if (attr2.hasOwnProperty(k)) {
          o[k] = attr2[k];
        }
      }
      return o;
    };
  }
  """ :: Attr -> Attr -> Attr

instance semigroupAttr :: Semigroup Attr where
  (<>) = appendAttr
  
instance monoidAttr :: Monoid Attr where
  mempty = emptyAttr