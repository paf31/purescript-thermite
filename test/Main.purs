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
  = ChangeCompleted Boolean
  | RemoveTask

-- | The state for a single task component
type Task = { completed :: Boolean, description :: String }

initialTask :: String -> Task
initialTask s = { completed: false, description: s }

-- | A `Spec` for a task component.
taskSpec :: forall eff props. T.Spec eff Task props TaskAction
taskSpec = T.simpleSpec performAction render
  where
  -- Renders the current state of the component as a collection of React elements.
  render :: T.Render Task props TaskAction
  render dispatch _ s _ =
    [ R.tr' <<< map (R.td' <<< pure) $
        [ R.input [ RP._type "checkbox"
                  , RP.className "checkbox"
                  , RP.checked (if s.completed then "checked" else "")
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
  performAction (ChangeCompleted b)   _ state k = k $ state { completed = b }
  performAction _                     _ _ _ = pure unit

-- | The three filters which can be applied to the list of tasks.
data Filter = All | Active | Completed

instance eqFilter :: Eq Filter where
  eq All       All       = true
  eq Active    Active    = true
  eq Completed Completed = true
  eq _         _         = false

showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"

-- | An action for the full task list component
data TaskListAction
  = NewTask String
  | SetEditText String
  | SetFilter Filter
  | TaskAction Int TaskAction

-- | A `Prism` which corresponds to the `TaskAction` constructor.
_TaskAction :: PrismP TaskListAction (Tuple Int TaskAction)
_TaskAction = prism (uncurry TaskAction) \ta ->
  case ta of
    TaskAction i a -> Right (Tuple i a)
    _ -> Left ta

-- | The state for the full task list component is a list of tasks
type TaskList =
  { tasks       :: List Task
  , editText    :: String
  , filter      :: Filter
  }

initialTaskList :: TaskList
initialTaskList =
  { tasks: Nil
  , editText: ""
  , filter: All
  }

-- | A `Lens` which corresponds to the `tasks` property.
_tasks :: LensP TaskList (List Task)
_tasks = lens _.tasks (_ { tasks = _ })

-- | A function which wraps a `Spec`'s `Render` function with a `container` element.
container :: forall state props action eff. T.Spec eff state props action -> T.Spec eff state props action
container = over T._render \render d p s c ->
  [ R.div [ RP.className "container" ] (render d p s c) ]

-- | A `Spec` for a component consisting of a `List` of tasks.
-- |
-- | This component is built up from smaller components: a header, a list of task components, and a footer.
-- | Lens and monoid combinators are used to compose them.
-- |
-- | Take note of the different state and action types for each component.
taskList :: forall props eff. T.Spec eff TaskList props TaskListAction
taskList = container $ fold
  [ header
  , table $ T.withState \st -> T.focus _tasks _TaskAction (T.foreach \_ -> applyFilter st.filter taskSpec)
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
      , R.div [ RP.className "btn-group" ] (map filter_ [ All, Active, Completed ])
      ]
      where
      filter_ :: Filter -> R.ReactElement
      filter_ f = R.button [ RP.className (if f == s.filter then "btn toolbar active" else "btn toolbar")
                           , RP.onClick \_ -> dispatch (SetFilter f)
                           ]
                           [ R.text (showFilter f) ]

    -- The `NewTask` action is handled here
    -- Everything else is handled by some other child component so is ignored here.
    performAction :: T.PerformAction eff TaskList props TaskListAction
    performAction (NewTask s) _ state k = k $ state { tasks = Cons (initialTask s) state.tasks }
    performAction _ _ _ _ = pure unit

  -- This function wraps a `Spec`'s `Render` function to filter out tasks.
  applyFilter :: forall props action eff. Filter -> T.Spec eff Task props action -> T.Spec eff Task props action
  applyFilter filter = over T._render \render d p s c ->
    if matches filter s
      then render d p s c
      else []
    where
    matches All       _ = true
    matches Completed t = t.completed
    matches Active    t = not t.completed

  -- This function wraps a `Spec`'s `Render` function in a table with the correct row headers.
  table :: forall props eff. T.Spec eff TaskList props TaskListAction -> T.Spec eff TaskList props TaskListAction
  table = over T._render \render dispatch p s c ->
    let handleKeyPress :: Int -> String -> _
        handleKeyPress 13 text = dispatch $ NewTask text
        handleKeyPress 27 _    = dispatch $ SetEditText ""
        handleKeyPress _  _    = pure unit
    in [ R.table [ RP.className "table table-striped" ]
                 [ R.thead' [ R.th [ RP.className "col-md-1"  ] []
                            , R.th [ RP.className "col-md-10" ] [ R.text "Description" ]
                            , R.th [ RP.className "col-md-1"  ] []
                            ]
                 , R.tbody' $ [ R.tr' [ R.td' []
                                      , R.td' [ R.input [ RP.className "form-control"
                                                        , RP.placeholder "Create a new task"
                                                        , RP.value s.editText
                                                        , RP.onKeyUp \e -> handleKeyPress (unsafeCoerce e).keyCode (unsafeCoerce e).target.value
                                                        , RP.onChange \e -> dispatch (SetEditText (unsafeCoerce e).target.value)
                                                        ] []
                                              ]
                                      , R.td' []
                                      ]
                              ] <> render dispatch p s c
                 ]
       ]

  -- The footer uses `defaultPerformAction` since it neither produces nor handles actions.
  -- It simply displays a label with information about completed tasks.
  footer :: forall action. T.Spec eff TaskList props action
  footer = T.simpleSpec T.defaultPerformAction \_ _ s _ ->
    let
      footerText = show completed ++ "/" ++ show total ++ " tasks completed."
      completed  = length $ filter _.completed s.tasks
      total      = length s.tasks
    in [ R.p' [ R.text footerText ] ]

  -- This `Spec` handles `RemoveTask` actions from child components
  listActions :: T.Spec eff TaskList props TaskListAction
  listActions = T.simpleSpec performAction T.defaultRender
    where
    performAction :: T.PerformAction eff TaskList props TaskListAction
    performAction (TaskAction i RemoveTask) _ state k = k $ state { tasks = fromMaybe state.tasks (deleteAt i state.tasks) }
    performAction (SetEditText s)           _ state k = k $ state { editText = s }
    performAction (SetFilter f)             _ state k = k $ state { filter = f }
    performAction _ _ _ _ = pure unit

-- | The main method creates the task list component, and renders it to the document body.
main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass taskList initialTaskList
  document <- DOM.window >>= DOM.document
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  R.render (R.createFactory component {}) container
