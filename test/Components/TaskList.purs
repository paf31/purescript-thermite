module Components.TaskList where

import Prelude
import Thermite as T
import Components.Task (Task, TaskAction(..), taskSpec, initialTask)
import Data.Either (Either(..))
import Data.Filter (Filter(..), showFilter)
import Data.Foldable (fold)
import Data.Lens (Lens', Prism', over, lens, prism)
import Data.List (List(..), deleteAt, length, filter)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import React (ReactElement) as R
import React.DOM (text, p', td', input, tr', tbody', th, thead', table, div, h1', button) as R
import React.DOM.Props as RP
import React.SyntheticEvent (SyntheticKeyboardEvent, SyntheticUIEvent, target, keyCode) as R
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- | An action for the full task list component
data TaskListAction
  = NewTask String
  | SetEditText String
  | SetFilter Filter
  | TaskAction Int TaskAction

-- | Nullary props
type Props = T.WithChildren ()

-- | A `Prism` which corresponds to the `TaskAction` constructor.
_TaskAction :: Prism' TaskListAction (Tuple Int TaskAction)
_TaskAction = prism (uncurry TaskAction) \ta ->
  case ta of
    TaskAction i a -> Right (Tuple i a)
    _ -> Left ta

-- | The state for the full task list component is a list of tasks
type TaskListState =
  { tasks       :: List Task
  , editText    :: String
  , filter      :: Filter
  }

initialTaskListState :: TaskListState
initialTaskListState =
  { tasks: Nil
  , editText: ""
  , filter: All
  }

-- | A `Lens` which corresponds to the `tasks` property.
_tasks :: Lens' TaskListState (List Task)
_tasks = lens _.tasks (_ { tasks = _ })

-- | A `Spec` for a component consisting of a `List` of tasks.
-- |
-- | This component is built up from smaller components: a header, a list of task components, and a footer.
-- | Lens and monoid combinators are used to compose them.
-- |
-- | Take note of the different state and action types for each component.
taskList :: T.Spec TaskListState Props TaskListAction
taskList = container $ fold
    [ header
    , table $ T.withState \st ->
        T.focus _tasks _TaskAction $
          T.foreach \_ -> applyFilter st.filter taskSpec
    , footer
    , listActions
    ]
  where
    -- | A function which wraps a `Spec`'s `Render` function with a `container` element.
    container :: forall state action. T.Spec state Props action -> T.Spec state Props action
    container = over T._render \render d p s c ->
      [ R.div [ RP.className "container" ] (render d p s c) ]

    -- The header component contains a button which will create a new task.
    header :: T.Spec TaskListState Props TaskListAction
    header = T.simpleSpec performAction render
      where
        render :: T.Render TaskListState Props TaskListAction
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
        performAction :: T.PerformAction TaskListState Props TaskListAction
        performAction (NewTask s) _ _ = void $ T.modifyState $ \state ->
          state { tasks = Cons (initialTask s) state.tasks
                , editText = ""
                }
        performAction _ _ _ = pure unit

    -- This function wraps a `Spec`'s `Render` function to filter out tasks.
    applyFilter :: forall action. Filter -> T.Spec Task Props action -> T.Spec Task Props action
    applyFilter filter = over T._render \render d p s c ->
      if matches filter s
        then render d p s c
        else []
      where
      matches All       _ = true
      matches Completed t = t.completed
      matches Active    t = not t.completed

    -- This function wraps a `Spec`'s `Render` function in a table with the correct row headers.
    table :: T.Spec TaskListState Props TaskListAction -> T.Spec TaskListState Props TaskListAction
    table = over T._render \render dispatch p s c ->
      let handleKeyPress :: R.SyntheticKeyboardEvent -> Effect Unit
          handleKeyPress e = do
            k <- R.keyCode e
            case unit of
              _ | k == 27.0 -> dispatch (SetEditText "")
                | k == 13.0 -> do
                  t <- R.target e
                  dispatch (NewTask (unsafeCoerce t).value)
                | otherwise -> pure unit
          setValue :: R.SyntheticUIEvent -> Effect Unit
          setValue e = do
            t <- R.target e
            dispatch (SetEditText (unsafeCoerce t).value)
          -- handleKeyPress 13 text = dispatch $ NewTask text
          -- handleKeyPress 27 _    = dispatch $ SetEditText ""
          -- handleKeyPress _  _    = pure unit
      in [ R.table [ RP.className "table table-striped" ]
                   [ R.thead' $ [ R.tr' [ R.th [ RP.className "col-md-1"  ] []
                                        , R.th [ RP.className "col-md-10" ] [ R.text "Description" ]
                                        , R.th [ RP.className "col-md-1"  ] []
                                        ]
                                ]
                   , R.tbody' $ [ R.tr' [ R.td' []
                                        , R.td' [ R.input [ RP.className "form-control"
                                                          , RP.placeholder "Create a new task"
                                                          , RP.value s.editText
                                                          , RP.onKeyUp handleKeyPress
                                                          , RP.onChange setValue
                                                          ]
                                                ]
                                        , R.td' []
                                        ]
                                ] <> render dispatch p s c
                   ]
         ]

    -- The footer uses `defaultPerformAction` since it neither produces nor handles actions.
    -- It simply displays a label with information about completed tasks.
    footer :: forall action. T.Spec TaskListState Props action
    footer = T.simpleSpec T.defaultPerformAction \_ _ s _ ->
      let
        footerText = show completed <> "/" <> show total <> " tasks completed."
        completed  = length $ filter _.completed s.tasks
        total      = length s.tasks
      in [ R.p' [ R.text footerText ] ]

    -- This `Spec` handles `RemoveTask` actions from child components
    listActions :: T.Spec TaskListState Props TaskListAction
    listActions = T.simpleSpec performAction T.defaultRender
      where
      performAction :: T.PerformAction TaskListState Props TaskListAction
      performAction (TaskAction i RemoveTask) _ _ = void $ T.modifyState \state -> state { tasks = fromMaybe state.tasks (deleteAt i state.tasks) }
      performAction (SetEditText s)           _ _ = void $ T.modifyState (_ { editText = s })
      performAction (SetFilter f)             _ _ = void $ T.modifyState (_ { filter = f })
      performAction _                         _ _ = pure unit
