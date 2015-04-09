-- | This module defines helper functions for creating event handlers.

module Thermite.Events where

import Thermite.Types
import Thermite.Internal

foreign import data ClipboardEvent :: *

onCopy :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
onCopy = event "onCopy"

onCut :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
onCut = event "onCut"

onPaste :: forall state props action. Context state action -> (ClipboardEvent -> action) -> Attr
onPaste = event "onPaste"

foreign import data KeyboardEvent :: *

onKeyDown :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
onKeyDown = event "onKeyDown"

onKeyPress :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
onKeyPress = event "onKeyPress"

onKeyUp :: forall state props action. Context state action -> (KeyboardEvent -> action) -> Attr
onKeyUp = event "onKeyUp"

foreign import data FocusEvent :: *

onFocus :: forall state props action. Context state action -> (FocusEvent -> action) -> Attr
onFocus = event "onFocus"

onBlur :: forall state props action. Context state action -> (FocusEvent -> action) -> Attr
onBlur = event "onBlur"

foreign import data FormEvent :: *

onChange :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
onChange = event "onChange"

onInput :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
onInput = event "onInput"

onSubmit :: forall state props action. Context state action -> (FormEvent -> action) -> Attr
onSubmit = event "onSubmit"

foreign import data MouseEvent :: *

onClick :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onClick = event "onClick"

onDoubleClick :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDoubleClick = event "onDoubleClick"

onDrag :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDrag = event "onDrag"

onDragEnd :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDragEnd = event "onDragEnd"

onDragEnter :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDragEnter = event "onDragEnter"

onDragExit :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDragExit = event "onDragExit"

onDragLeave :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDragLeave = event "onDragLeave"

onDragOver :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDragOver = event "onDragOver"

onDragStart :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDragStart = event "onDragStart"

onDrop :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onDrop = event "onDrop"

onMouseDown :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onMouseDown = event "onMouseDown"

onMouseEnter :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onMouseEnter = event "onMouseEnter"

onMouseLeave :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onMouseLeave = event "onMouseLeave"

onMouseMove :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onMouseMove = event "onMouseMove"

onMouseOut :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onMouseOut = event "onMouseOut"

onMouseOver :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onMouseOver = event "onMouseOver"

onMouseUp :: forall state props action. Context state action -> (MouseEvent -> action) -> Attr
onMouseUp = event "onMouseUp"

foreign import data TouchEvent :: *

onTouchCancel :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
onTouchCancel = event "onTouchCancel"

onTouchEnd :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
onTouchEnd = event "onTouchEnd"

onTouchMove :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
onTouchMove = event "onTouchMove"

onTouchStart :: forall state props action. Context state action -> (TouchEvent -> action) -> Attr
onTouchStart = event "onTouchStart"

foreign import data UIEvent :: *

onScroll :: forall state props action. Context state action -> (UIEvent -> action) -> Attr
onScroll = event "onScroll"

foreign import data WheelEvent :: *

onWheel :: forall state props action. Context state action -> (WheelEvent -> action) -> Attr
onWheel = event "onWheel"
