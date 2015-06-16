## Module Thermite.Internal

#### `getStateImpl`

``` purescript
getStateImpl :: forall eff state props action. Context state action -> Eff eff state
```

#### `setStateImpl`

``` purescript
setStateImpl :: forall eff state props action. Context state action -> state -> Eff eff Unit
```

#### `textImpl`

``` purescript
textImpl :: forall eff. String -> Html eff
```

#### `createElementImpl`

``` purescript
createElementImpl :: forall element props eff. element -> props -> Array (Html eff) -> Html eff
```

#### `unsafeAttribute`

``` purescript
unsafeAttribute :: forall attr. String -> attr -> Attr
```

#### `event`

``` purescript
event :: forall state action event. String -> Context state action -> (event -> action) -> Attr
```

#### `documentBody`

``` purescript
documentBody :: forall props eff. Eff (dom :: DOM | eff) Node
```

#### `renderToImpl`

``` purescript
renderToImpl :: forall props eff. Node -> ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
```


