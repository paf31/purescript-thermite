module Thermite.Html 
  ( createElement  
  , text
  ) where

import Thermite.Types
import Thermite.Internal

text :: forall action. String -> Html action
text = textImpl

createElement :: forall action. String -> Attr action -> [Html action] -> Html action
createElement = createElementImpl
