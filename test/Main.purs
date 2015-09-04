module Test.Main (main) where

import Prelude

import Data.Maybe
import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)

import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Types as DOM

data Action = Increment | Decrement

type State = { counter :: Int }

initialState :: State
initialState = { counter: 0 }

render :: T.Render _ State _ Action
render dispatch s _ _ = R.div' [counter, buttons]
  where
  counter :: R.ReactElement
  counter =
    R.p'
      [ R.text "Value: "
      , R.text $ show s.counter
      ]

  buttons :: R.ReactElement
  buttons =
    R.p'
      [ R.button [ RP.onClick (\_ -> dispatch Increment) ]
                 [ R.text "Increment" ]
      , R.button [ RP.onClick (\_ -> dispatch Decrement) ]
                 [ R.text "Decrement" ]
      ]

performAction :: T.PerformAction _ State _ Action
performAction _ Increment = T.modifyState \o -> { counter: o.counter + 1 }
performAction _ Decrement = T.modifyState \o -> { counter: o.counter - 1 }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount Increment

main =
  let component = T.createClass spec in
  body >>= R.render (R.createFactory component {}) 
  
  where
  body :: forall eff. Eff (dom :: DOM.DOM | eff) DOM.Element
  body = do
    win <- DOM.window
    doc <- DOM.document win
    elm <- fromJust <$> toMaybe <$> DOM.body doc
    return $ DOM.htmlElementToElement elm
