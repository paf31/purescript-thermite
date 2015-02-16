module Thermite.Html 
  ( createElement
  , createComponent
  , text
  ) where

import Thermite.Types
import Thermite.Internal

text :: forall action. String -> Html action
text = textImpl

createElement :: forall action. String -> Props action -> [Html action] -> Html action
createElement = createElementImpl

createComponent :: forall eff action. ComponentClass action eff -> Props action -> Html action
createComponent = createComponentImpl
