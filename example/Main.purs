module Main (main) where

import Data.Function

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

data Action = TextChanged String | ClearText

data State = State { name :: String }

data Props = Props { greeting :: String }

foreign import unsafeStringValue 
  "function unsafeStringValue(e) {\
  \  return e.target.value;\
  \}" :: T.FormEvent -> String

initialState :: State
initialState = State { name: "" }

render :: T.Render State Props Action
render ctx (State s) (Props p) = T.div' $ welcome : response s.name
  where
  welcome :: T.Html _
  welcome = 
    T.div'
      [ T.input [ A.value s.name
                , A.placeholder "What is your name?"
                , T.onChange ctx (TextChanged <<< unsafeStringValue) 
                ] []
      , T.button [ T.onClick ctx (\_ -> TextChanged "") ] 
                 [ T.text "Clear" ]
      ]

  response :: String -> [T.Html _]
  response "" = []
  response s = 
    [ T.div'
      [ T.text p.greeting
      , T.text s
      ]
    ]

performAction :: T.PerformAction Props Action (T.Action _ State) 
performAction _ (TextChanged s) = T.setState $ State { name: s }

spec :: T.Spec _ State Props Action
spec = T.Spec { initialState: initialState
              , performAction: performAction
              , render: render
              }

main = do
  let component = T.createClass spec
  T.render component (Props { greeting: "Hello, " })
