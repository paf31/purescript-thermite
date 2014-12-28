module Thermite.Action 
  ( ActionResult()
  , Action()
  , action
  , runAction
  , getState
  , setState
  , modifyState
  , asyncSetState
  ) where

import Control.Monad.Eff

type ActionResult state a = { state :: state, value :: a }

newtype Action eff state a = Action (state -> (ActionResult state a -> Eff eff Unit) -> Eff eff Unit)

runAction :: forall eff state a. Action eff state a -> state -> (ActionResult state a -> Eff eff Unit) -> Eff eff Unit
runAction (Action act) = act

action :: forall eff state a. (state -> (a -> state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state a
action act = Action \s k -> act s \a s1 -> k { state: s1, value: a }

getState :: forall eff state. Action eff state state
getState = action \s k -> k s s

setState :: forall eff state. state -> Action eff state Unit
setState s = action \_ k -> k unit s

modifyState :: forall eff state. (state -> state) -> Action eff state Unit
modifyState f = action \s k -> k unit (f s)

asyncSetState :: forall eff state. ((state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state Unit
asyncSetState act = action \s k -> act \s1 -> k unit s1

instance functorAction :: Functor (Action eff state) where
  (<$>) f (Action act) = Action \s k -> act s \o -> k { state: o.state, value: f o.value }

instance applyAction :: Apply (Action eff state) where
  (<*>) = ap

instance applicativeAction :: Applicative (Action eff state) where
  pure a = action \s k -> k a s

instance bindAction :: Bind (Action eff state) where
  (>>=) (Action act) f = Action \s k -> act s \o ->
                           case f o.value of
                             Action act1 -> act1 o.state k

instance monadAction :: Monad (Action eff state)
