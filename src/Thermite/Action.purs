module Thermite.Action 
  ( ActionResult()
  , Action()
  , runAction
  , getState
  , setState
  , modifyState
  , asyncSetState
  ) where

import Control.Monad.Eff

import Thermite.Types
import Thermite.Internal

type ActionResult state a = { state :: state, value :: a }

data ActionF eff state next
  = GetState (state -> next)
  | SetState state next
  | Wait ((next -> Eff eff Unit) -> Eff eff Unit)

instance functorActionF :: Functor (ActionF eff state) where
  (<$>) f (GetState k) = GetState (f <<< k)
  (<$>) f (SetState s a) = SetState s (f a)
  (<$>) f (Wait c) = Wait \k -> c (k <<< f)

data Action eff state a = Pure a | Impure (ActionF eff state (Action eff state a))

runAction :: forall eff state props action a. Context state props action -> Action eff state a -> Eff eff Unit 
runAction ctx = go
  where
  go (Pure _) = return unit
  go (Impure (GetState k)) = void do
    s <- getStateImpl ctx
    go (k s)
  go (Impure (SetState s next)) = void do
    setStateImpl ctx s
    go next
  go (Impure (Wait c)) = c go

getState :: forall eff state. Action eff state state
getState = Impure $ GetState Pure

setState :: forall eff state. state -> Action eff state Unit
setState s = Impure $ SetState s (Pure unit)

modifyState :: forall eff state. (state -> state) -> Action eff state Unit
modifyState f = do
  s <- getState
  setState (f s)

wait :: forall eff state a. ((a -> Eff eff Unit) -> Eff eff Unit) -> Action eff state a
wait c = Impure $ Wait \k -> c (k <<< Pure)

asyncSetState :: forall eff state. ((state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state Unit
asyncSetState c = do
  s <- wait c 
  setState s 

instance functorAction :: Functor (Action eff state) where
  (<$>) f (Pure a) = Pure (f a)
  (<$>) f (Impure x) = Impure ((<$>) f <$> x)

instance applyAction :: Apply (Action eff state) where
  (<*>) = ap

instance applicativeAction :: Applicative (Action eff state) where
  pure = Pure

instance bindAction :: Bind (Action eff state) where
  (>>=) (Pure a) f = f a
  (>>=) (Impure x) f = Impure $ x <#> \a -> a >>= f 

instance monadAction :: Monad (Action eff state)
