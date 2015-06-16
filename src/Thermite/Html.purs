-- | This module defines functions for creating simple HTML documents.

module Thermite.Html 
  ( text
  , createElement  
  , component
  ) where

import Prelude

import Thermite.Types
import Thermite.Internal

-- | Create a text node.
text :: forall eff. String -> Html eff
text = textImpl

-- | Create a HTML element from a tag name, a set of attributes and a collection of child nodes.
createElement :: forall eff. String -> Attr -> Array (Html eff) -> Html eff
createElement = createElementImpl

-- | Create a HTML document from a component class
component :: forall props eff eff. ComponentClass props eff -> props -> Array (Html eff) -> Html eff
component = createElementImpl
