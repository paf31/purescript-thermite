module Main (main) where

import Data.Function

import qualified Thermite as T
import qualified Thermite.Html as T

data Action = TextChanged String

data State = State { name :: String }

data Props = Props { greeting :: String }

initialState :: State
initialState = State { name: "" }

render :: T.Render State Props Action
render = mkFn3 render'
  where
  render' :: T.Context Action -> State -> Props -> T.Html Action
  render' ctx (State s) (Props p) = T.createElement "div" (T.props []) $ welcome : response s.name
    where
    welcome :: T.Html _
    welcome = 
      T.createElement "div" (T.props [])
        [ T.text "What is your name? "
        , T.createElement "input" (T.props [ T.onChange ctx TextChanged ]) []
        ]

    response :: String -> [T.Html _]
    response "" = []
    response s = 
      [ T.createElement "div" (T.props [])
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
