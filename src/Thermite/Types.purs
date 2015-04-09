-- | This module defines types used by the Thermite library.

module Thermite.Types 
  ( Context()
  , ComponentClass()
  , Attr()
  , Html()
  , PerformAction()
  , Render()
  , Spec(..)
  , SpecRecord()
  ) where

import Data.Maybe
import Data.Monoid

-- | The `Context` type represents React's `this` reference.
-- |
-- | It is passed to event handlers to allow us to get and set component state.
data Context state props action

-- | A component class, the result of React's `createClass` method.
-- |
-- | The type parameters capture the properties required by the class, and the effects
-- | its action handlers can have.
data ComponentClass props (eff :: # !)

-- | The type of HTML attributes, parameterised by the type of actions they can emit from 
-- | event handlers.
data Attr action

-- | The type of HTML elements.
data Html action

-- | A type synonym for action handlers, which take an action and the current properties
-- | for the component, and return a computation in some monad `m`.
-- |
-- | In practice, `m` will be the `Action` monad.
type PerformAction props action m = props -> action -> m Unit

-- | A rendering function, which takes a `Context`, the current state and properties, and 
-- | returns a HTML document.
type Render state props action = Context state props action -> state -> props -> Html action

-- | A component specification, which can be passed to `createClass`.
-- | 
-- | A minimal `Spec` can be built using `simpleSpec`, and extended with optional arguments
-- | using functions in the `Thermite` module.
newtype Spec m state props action = Spec (SpecRecord m state props action)

-- | A type synonym for the record type which captures the functions which make up a `Spec`.
type SpecRecord m state props action =
  { initialState       :: state
  , performAction      :: PerformAction props action m
  , render             :: Render state props action
  , componentWillMount :: Maybe action
  , displayName        :: Maybe String
  }
  
foreign import emptyAttr """
  var emptyAttr = {};
  """ :: forall action. Attr action

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
  """ :: forall action. Attr action -> Attr action -> Attr action

instance semigroupAttr :: Semigroup (Attr action) where
  (<>) = appendAttr
  
instance monoidAttr :: Monoid (Attr action) where
  mempty = emptyAttr