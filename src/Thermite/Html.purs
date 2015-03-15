module Thermite.Html
  ( createElement
  , text
  ) where

import Thermite.Types
import Thermite.Internal

text :: forall action. String -> Html action
text = textImpl

createElement :: forall action. String -> Props action -> [Html action] -> Html action
createElement = createElementFromTagName
