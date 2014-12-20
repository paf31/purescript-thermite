module Main (main) where

import Data.Maybe

import qualified Thermite as T
import qualified Thermite.Html as T

data Action = TextChanged String

data State = State String

initialState :: State
initialState = State ""

render :: T.Context Action -> State -> T.Html _
render ctx (State s) = 
  T.createElement "div" (T.props []) $ welcome : response s
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
        [ T.text "Hello, "
        , T.text s 
        ]
    ]

performAction :: State -> Action -> T.Action _ State
performAction _ (TextChanged s) k = k (State s)

spec :: T.Spec _ State Action
spec = T.Spec { initialState: initialState
              , setup: Nothing
              , performAction: performAction
              , render: render
              }

main = do
  let component = T.createClass spec
  T.render component
