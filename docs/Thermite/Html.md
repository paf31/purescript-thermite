## Module Thermite.Html

This module defines functions for creating simple HTML documents.

#### `text`

``` purescript
text :: forall eff. String -> Html eff
```

Create a text node.

#### `createElement`

``` purescript
createElement :: forall eff. String -> Attr -> Array (Html eff) -> Html eff
```

Create a HTML element from a tag name, a set of attributes and a collection of child nodes.

#### `component`

``` purescript
component :: forall props eff eff. ComponentClass props eff -> props -> Array (Html eff) -> Html eff
```

Create a HTML document from a component class


