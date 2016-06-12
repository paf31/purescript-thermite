module Data.Filter where

import Prelude

-- | The three filters which can be applied to the list of tasks.
data Filter = All | Active | Completed

derive instance eqFilter :: Eq Filter

showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"
