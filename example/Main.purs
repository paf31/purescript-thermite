module Main (main) where

import Data.Function

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

data Action = Increment | Decrement

type State = { counter :: Number }

initialState :: State
initialState = { counter: 0 }

render :: T.Render State _ Action
render ctx s _ = T.div' [counter, buttons]
  where
  counter :: T.Html _
  counter = 
    T.p'
      [ T.text "Value: "
      , T.text $ show s.counter
      ]

  buttons :: T.Html _
  buttons = 
    T.p'
      [ T.button [ T.onClick ctx (\_ -> Increment) ] 
                 [ T.text "Increment" ]
      , T.button [ T.onClick ctx (\_ -> Decrement) ] 
                 [ T.text "Decrement" ]
      ]

performAction :: T.PerformAction _ Action (T.Action _ State) 
performAction _ Increment = T.modifyState \o -> { counter: o.counter + 1 }
performAction _ Decrement = T.modifyState \o -> { counter: o.counter - 1 }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount Increment

main = do
  let component = T.createClass spec
  T.render component {}
