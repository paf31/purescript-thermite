-- | This module defines functions for creating simple HTML documents.

module Thermite.Html 
  ( createElement  
  , text
  ) where

import Thermite.Types
import Thermite.Internal

-- | Create a text node.
text :: forall action. String -> Html action
text = textImpl

-- | Create a HTML element from a tag name, a set of attributes and a collection of child nodes.
createElement :: forall action. String -> Attr action -> [Html action] -> Html action
createElement = createElementImpl
