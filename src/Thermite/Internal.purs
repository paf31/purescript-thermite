module Thermite.Internal where

import Prelude

import DOM

import Data.Maybe

import Control.Monad.Eff

import Thermite.Types

foreign import getStateImpl :: forall eff state props action. Context state action -> Eff eff state

foreign import setStateImpl :: forall eff state props action. Context state action -> state -> Eff eff Unit

foreign import textImpl :: forall eff. String -> Html eff

foreign import createElementImpl :: forall element props eff. element -> props -> Array (Html eff) -> Html eff

foreign import unsafeAttribute :: forall attr. String -> attr -> Attr

foreign import event :: forall state action event. String -> Context state action -> (event -> action) -> Attr

foreign import documentBody :: forall props eff. Eff (dom :: DOM | eff) Node

foreign import renderToImpl :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
