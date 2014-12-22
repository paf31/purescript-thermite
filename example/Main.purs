module Main (main) where

import Data.Function

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Events as T

data Action = TextChanged String | ClearText

data State = State { name :: String }

data Props = Props { greeting :: String }

initialState :: State
initialState = State { name: "" }

render :: T.Render State Props Action
render = mkFn3 render'
  where
  render' :: T.Context Action -> State -> Props -> T.Html Action
  render' ctx (State s) (Props p) = T.div' $ welcome : response s.name
    where
    welcome :: T.Html _
    welcome = 
      T.div'
        [ T.text "What is your name? "
        , T.input [ T.onChange ctx TextChanged ] []
        , T.button [ T.onClick ctx (TextChanged "") ] [ T.text "Clear" ]
        ]

    response :: String -> [T.Html _]
    response "" = []
    response s = 
      [ T.div'
          [ T.text p.greeting
          , T.text s
          ]
      ]

performAction :: T.PerformAction State Props Action _ 
performAction = mkFn3 \_ _ (TextChanged s) k -> k (State { name: s })

spec :: T.Spec _ State Props Action
spec = T.Spec { initialState: initialState
              , performAction: performAction
              , render: render
              }

main = do
  let component = T.createClass spec
  T.render component (Props { greeting: "Hello, " })
