## Module Thermite.Events

This module defines helper functions for creating event handlers.

#### `ClipboardEvent`

``` purescript
data ClipboardEvent :: *
```

#### `onCopy`

``` purescript
onCopy :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
```

#### `onCut`

``` purescript
onCut :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
```

#### `onPaste`

``` purescript
onPaste :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
```

#### `KeyboardEvent`

``` purescript
data KeyboardEvent :: *
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
```

#### `FocusEvent`

``` purescript
data FocusEvent :: *
```

#### `onFocus`

``` purescript
onFocus :: forall state props action. Context state action -> (FocusEvent -> action) -> Attr
```

#### `onBlur`

``` purescript
onBlur :: forall state props action. Context state action -> (FocusEvent -> action) -> Attr
```

#### `FormEvent`

``` purescript
data FormEvent :: *
```

#### `onChange`

``` purescript
onChange :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
```

#### `onInput`

``` purescript
onInput :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
```

#### `onSubmit`

``` purescript
onSubmit :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
```

#### `MouseEvent`

``` purescript
data MouseEvent :: *
```

#### `onClick`

``` purescript
onClick :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDrag`

``` purescript
onDrag :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDragEnd`

``` purescript
onDragEnd :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDragEnter`

``` purescript
onDragEnter :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDragExit`

``` purescript
onDragExit :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDragLeave`

``` purescript
onDragLeave :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDragOver`

``` purescript
onDragOver :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDragStart`

``` purescript
onDragStart :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onDrop`

``` purescript
onDrop :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
```

#### `TouchEvent`

``` purescript
data TouchEvent :: *
```

#### `onTouchCancel`

``` purescript
onTouchCancel :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```

#### `onTouchEnd`

``` purescript
onTouchEnd :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```

#### `onTouchMove`

``` purescript
onTouchMove :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```

#### `onTouchStart`

``` purescript
onTouchStart :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
```

#### `UIEvent`

``` purescript
data UIEvent :: *
```

#### `onScroll`

``` purescript
onScroll :: forall state props action. Context state action -> (UIEvent -> action) -> Attr
```

#### `WheelEvent`

``` purescript
data WheelEvent :: *
```

#### `onWheel`

``` purescript
onWheel :: forall state props action. Context state action -> (WheelEvent -> action) -> Attr
```


