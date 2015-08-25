module Test.Main (main) where

import Prelude

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

data Action = Increment | Decrement

type State = { counter :: Int }

initialState :: State
initialState = { counter: 0.0 }

render :: T.Render _ State _ Action
render ctx s _ _ = T.div' [counter, buttons]
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
      [ T.button (T.onClick ctx (\_ -> Increment))
                 [ T.text "Increment" ]
      , T.button (T.onClick ctx (\_ -> Decrement))
                 [ T.text "Decrement" ]
      ]

performAction :: T.PerformAction _ State _ Action
performAction _ Increment = T.modifyState \o -> { counter: o.counter + 1.0 }
performAction _ Decrement = T.modifyState \o -> { counter: o.counter - 1.0 }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount Increment

main = do
  let component = T.createClass spec
  T.render component {}
