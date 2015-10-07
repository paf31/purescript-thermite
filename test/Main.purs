-- | This task list application is made up of two Thermite components:
-- |
-- | - a component for a single task
-- | - a component for a list of tasks
-- |
-- | This example demonstrates the main features of a Thermite app:
-- |
-- | - Using `simpleSpec` to create simple components from `Render` and
-- |   `PerformAction` functions
-- | - Composing components using the `Monoid` instance and lensy combinators.
-- |
-- | For each component we start by declaring action and state types.

module Test.Main (main) where

import Prelude

import Data.Lens
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)
import Data.Foldable (fold)

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe

import qualified Thermite as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM
import qualified DOM.Node.Types as DOM

import Unsafe.Coerce

-- | Actions for a single task component
data TaskAction
  = ChangeDescription String
  | ChangeCompleted Boolean
  | RemoveTask

-- | The state for a single task component
type Task = { completed :: Boolean, description :: String }

initialTask :: Task
initialTask = { completed: false, description: "" }

-- | A `Spec` for a task component.
taskSpec :: forall eff props. T.Spec eff Task props TaskAction
taskSpec = T.simpleSpec performAction render
  where
  -- Renders the current state of the component as a collection of React elements.
  render :: T.Render Task props TaskAction
  render dispatch _ s _ =
    [ R.div [ RP.className "form-inline" ]
            [ R.input [ RP._type "checkbox"
                      , RP.checked (if s.completed then "checked" else "")
                      , RP.onChange \e -> dispatch (ChangeCompleted (unsafeCoerce e).target.checked)
                      ] []
            , R.input [ RP.value s.description
                      , RP.onChange \e -> dispatch (ChangeDescription (unsafeCoerce e).target.value)
                      ] []
            , R.button [ RP.onClick (\_ -> dispatch RemoveTask) ]
                       [ R.text "Remove" ]
            ]
    ]

  -- Updates the state in response to an action.
  --
  -- _Note_: this component can only see actions of type `TaskAction`, but the `RemoveTask` action
  -- is ignored here: it will be handled by the parent component.
  performAction :: T.PerformAction eff Task props TaskAction
  performAction (ChangeCompleted b)   _ state k = k $ state { completed = b }
  performAction (ChangeDescription s) _ state k = k $ state { description = s }
  performAction _                     _ _ _ = pure unit

-- | An action for the full task list component
data TaskListAction = NewTask | TaskAction Int TaskAction

-- | The state for the full task list component is a list of tasks
type TaskList = List Task

-- | A `Prism` which corresponds to the `TaskAction` constructor.
_TaskAction :: PrismP TaskListAction (Tuple Int TaskAction)
_TaskAction = prism (uncurry TaskAction) \ta ->
  case ta of
    TaskAction i a -> Right (Tuple i a)
    _ -> Left ta

-- | A `Spec` for a component consisting of a `List` of tasks.
-- |
-- | This component is built up from smaller components: a header, a list of task components, and a footer.
-- | Lens and monoid combinators are used to compose them.
-- |
-- | Take note of the different state and action types for each component.
taskList :: forall props eff. T.Spec eff TaskList props TaskListAction
taskList = fold
  [ header
  , T.match _TaskAction (T.foreach \_ -> taskSpec)
  , footer
  , listActions
  ]
  where
  -- The header component contains a button which will create a new task.
  header :: T.Spec eff TaskList props TaskListAction
  header = T.simpleSpec performAction render
    where
    render :: T.Render TaskList props TaskListAction
    render dispatch _ s _ =
      [ R.h1' [ R.text "todo list" ]
      , R.p' [ R.button [ RP.onClick (\_ -> dispatch NewTask) ]
                        [ R.text "New task" ]
             ]
      ]

    -- The `NewTask` action is handled here
    -- Everything else is handled by some other child component so is ignored here.
    performAction :: T.PerformAction eff TaskList props TaskListAction
    performAction NewTask _ state k = k (Cons initialTask state)
    performAction _ _ _ _ = pure unit

  -- The footer uses `defaultPerformAction` since it neither produces nor handles actions.
  -- It simply displays a label with information about completed tasks.
  footer :: forall action. T.Spec eff TaskList props action
  footer = T.simpleSpec T.defaultPerformAction \_ _ s _ ->
    let
      footerText = show completed ++ "/" ++ show total ++ " tasks completed."
      completed  = length $ filter _.completed s
      total      = length s
    in [ R.p' [ R.text footerText ] ]

  -- This `Spec` handles `RemoveTask` actions from child components
  listActions :: T.Spec eff TaskList props TaskListAction
  listActions = T.simpleSpec performAction T.defaultRender
    where
    performAction :: T.PerformAction eff TaskList props TaskListAction
    performAction (TaskAction i RemoveTask) _ state k = k $ fromMaybe state (deleteAt i state)
    performAction _ _ _ _ = pure unit

-- | The main method creates the task list component, and renders it to the document body.
main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass taskList Nil
  document <- DOM.window >>= DOM.document
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  R.render (R.createFactory component {}) container
