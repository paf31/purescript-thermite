## Module Thermite.Types

This module defines types used by the Thermite library.

#### `Context`

``` purescript
data Context state action
```

The `Context` type represents React's `this` reference.

It is passed to event handlers to allow us to get and set component state.

#### `ComponentClass`

``` purescript
data ComponentClass props (eff :: # !)
```

A component class, the result of React's `createClass` method.

The type parameters capture the properties required by the class, and the effects
its action handlers can have.

#### `Attr`

``` purescript
data Attr
```

The type of HTML attributes.

##### Instances
``` purescript
instance semigroupAttr :: Semigroup Attr
instance monoidAttr :: Monoid Attr
```

#### `Html`

``` purescript
data Html (eff :: # !)
```

The type of HTML elements.


