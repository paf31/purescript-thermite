## Module Thermite.Action

This module defines the `Action` monad, which can be used to access the 
component state, and invoke asynchronous actions in the `Eff` monad.

#### `Action`

``` purescript
data Action eff state a
```

The `Action` monad, parameterized by 

- The row of effects which are allowed in its asynchronous actions
- The component state type
- The return type

##### Instances
``` purescript
instance functorAction :: Functor (Action eff state)
instance applyAction :: Apply (Action eff state)
instance applicativeAction :: Applicative (Action eff state)
instance bindAction :: Bind (Action eff state)
instance monadAction :: Monad (Action eff state)
instance monadEffAction :: MonadEff eff (Action eff state)
```

#### `runAction`

``` purescript
runAction :: forall eff state props a. ReactThis props state -> Action (state :: ReactState (Read Write) state | eff) state a -> Eff (state :: ReactState (Read Write) state | eff) Unit
```

Run a computation in the `Action` monad.

#### `getState`

``` purescript
getState :: forall eff state. Action eff state state
```

Get the current component state.

#### `setState`

``` purescript
setState :: forall eff state. state -> Action eff state Unit
```

Update the component state.

#### `modifyState`

``` purescript
modifyState :: forall eff state. (state -> state) -> Action eff state Unit
```

Modify the component state by applying a function.

#### `async`

``` purescript
async :: forall eff state a. ((a -> Eff eff Unit) -> Eff eff Unit) -> Action eff state a
```

Run an asynchronous computation.

The first argument is a function which takes a callback, and starts some asynchronous computation,
invoking the callback when the result is available.

#### `asyncSetState`

``` purescript
asyncSetState :: forall eff state. ((state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state Unit
```

Set the component state based on the result of an asynchronous computation.

The first argument is a function which takes a callback, and starts some asynchronous computation,
invoking the callback when the new state is available.


