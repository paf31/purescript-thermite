module Thermite.Types where

data Context state props action

data ComponentClass props (eff :: # !)

data Prop action

type Props action = [Prop action]

data Html action

data Mixin

type PerformAction props action m = props -> action -> m Unit

type Render state props action = Context state props action -> state -> props -> Html action

newtype Spec m state props action = Spec (SpecRecord m state props action)

type SpecRecord m state props action =
  { mixins        :: [Mixin]
  , initialState  :: state
  , performAction :: PerformAction props action m
  , render        :: Render state props action
  }

