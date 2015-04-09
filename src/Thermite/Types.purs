module Thermite.Types where

import Data.Maybe
import Data.Monoid

data Context state props action

data ComponentClass props (eff :: # !)

data Attr action

data Html action

type PerformAction props action m = props -> action -> m Unit

type Render state props action = Context state props action -> state -> props -> Html action

newtype Spec m state props action = Spec (SpecRecord m state props action)

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