module Components.Task where

import Prelude

import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

-- | Actions for the task component
data TaskAction
  = ChangeCompleted Boolean
  | RemoveTask

-- | The state for the task component
type Task =
  { completed :: Boolean
    , description :: String
  }

initialTask :: String -> Task
initialTask s = { completed: false, description: s }

-- | A `Spec` for the task component.
taskSpec :: forall eff props. T.Spec eff Task props TaskAction
taskSpec = T.simpleSpec performAction render
  where
  -- Renders the current state of the component as a collection of React elements.
  render :: T.Render Task props TaskAction
  render dispatch _ s _ =
    [ R.tr' <<< map (R.td' <<< pure) $
        [ R.input [ RP._type "checkbox"
                  , RP.className "checkbox"
                  , RP.checked s.completed
                  , RP.title "Mark as completed"
                  , RP.onChange \e -> dispatch (ChangeCompleted (unsafeCoerce e).target.checked)
                  ] []
        , R.text s.description
        , R.a [ RP.className "btn btn-danger pull-right"
              , RP.title "Remove item"
              , RP.onClick \_ -> dispatch RemoveTask
              ]
              [ R.text "✖" ]
        ]
    ]

  -- Updates the state in response to an action.
  --
  -- _Note_: this component can only see actions of type `TaskAction`, but the `RemoveTask` action
  -- is ignored here: it will be handled by the parent component.
  performAction :: T.PerformAction eff Task props TaskAction
  performAction (ChangeCompleted b)   _ _ = void do
    -- This is a test for issue #65.
    -- In practice, we only need one `modifyState` here.
    T.modifyState id
    T.modifyState (_ { completed = b })
  performAction _                     _ _ = pure unit
