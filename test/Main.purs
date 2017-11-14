-- | This task list application is made up of two Thermite components:
-- |
-- | - a component for a single task (Components.Task)
-- | - a component for a list of tasks (Components.TaskList)
-- |
-- | This example demonstrates the main features of a Thermite app:
-- |
-- | - Using `simpleSpec` to create simple components from `Render` and
-- |   `PerformAction` functions
-- | - Composing components using the `Monoid` instance and lens combinators.
-- |
-- | For each component we start by declaring action and state types.

module Test.Main (main) where

import Prelude
import Components.TaskList (initialTaskListState, taskList)
import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import Thermite as T

-- | The main method creates the task list component, and renders it to the document body.
main :: Eff (dom :: DOM.DOM) Unit
main = T.defaultMain taskList (const $ pure initialTaskListState) unit
