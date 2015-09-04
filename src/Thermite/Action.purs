-- | This module defines the `Action` monad, which can be used to access the 
-- | component state, and invoke asynchronous actions in the `Eff` monad.

module Thermite.Action 
  ( Action()
  , runAction
  , getState
  , setState
  , modifyState
  , async
  , asyncSetState
  ) where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Class

data ActionF eff state next
  = GetState (state -> next)
  | SetState state next
  | Wait ((next -> Eff eff Unit) -> Eff eff Unit)

instance functorActionF :: Functor (ActionF eff state) where
  map f (GetState k) = GetState (f <<< k)
  map f (SetState s a) = SetState s (f a)
  map f (Wait c) = Wait \k -> c (k <<< f)

-- | The `Action` monad, parameterized by 
-- |
-- | - The row of effects which are allowed in its asynchronous actions
-- | - The component state type
-- | - The return type
data Action eff state a = Pure a | Impure (ActionF eff state (Action eff state a))

-- | Run a computation in the `Action` monad.
runAction :: forall eff state props a.
               React.ReactThis props state -> 
               Action (state :: React.ReactState (React.Read React.Write) state | eff) state a -> 
               Eff    (state :: React.ReactState (React.Read React.Write) state | eff) Unit 
runAction this = go
  where
  go (Pure _) = return unit
  go (Impure (GetState k)) = void do
    s <- React.readState this
    go (k s)
  go (Impure (SetState s next)) = void do
    React.writeState this s
    go next
  go (Impure (Wait c)) = c go

-- | Get the current component state.
getState :: forall eff state. Action eff state state
getState = Impure $ GetState Pure

-- | Update the component state.
setState :: forall eff state. state -> Action eff state Unit
setState s = Impure $ SetState s (Pure unit)

-- | Modify the component state by applying a function.
modifyState :: forall eff state. (state -> state) -> Action eff state Unit
modifyState f = do
  s <- getState
  setState (f s)

-- | Run an asynchronous computation.
-- |
-- | The first argument is a function which takes a callback, and starts some asynchronous computation,
-- | invoking the callback when the result is available.
async :: forall eff state a. ((a -> Eff eff Unit) -> Eff eff Unit) -> Action eff state a
async c = Impure $ Wait \k -> c (k <<< Pure)

-- | Set the component state based on the result of an asynchronous computation.
-- |
-- | The first argument is a function which takes a callback, and starts some asynchronous computation,
-- | invoking the callback when the new state is available.
asyncSetState :: forall eff state. ((state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state Unit
asyncSetState c = do
  s <- async c 
  setState s 

instance functorAction :: Functor (Action eff state) where
  map f (Pure a) = Pure (f a)
  map f (Impure x) = Impure ((<$>) f <$> x)

instance applyAction :: Apply (Action eff state) where
  apply = ap

instance applicativeAction :: Applicative (Action eff state) where
  pure = Pure

instance bindAction :: Bind (Action eff state) where
  bind (Pure a) f = f a
  bind (Impure x) f = Impure $ x <#> \a -> a >>= f 

instance monadAction :: Monad (Action eff state)

instance monadEffAction :: MonadEff eff (Action eff state) where
  liftEff e = async ((>>=) e)
