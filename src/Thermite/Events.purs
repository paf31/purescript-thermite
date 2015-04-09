module Thermite.Events where

import Thermite.Types
import Thermite.Internal

foreign import data ClipboardEvent :: *

onCopy :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Attr action
onCopy = event "onCopy"

onCut :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Attr action
onCut = event "onCut"

onPaste :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Attr action
onPaste = event "onPaste"

foreign import data KeyboardEvent :: *

onKeyDown :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Attr action
onKeyDown = event "onKeyDown"

onKeyPress :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Attr action
onKeyPress = event "onKeyPress"

onKeyUp :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Attr action
onKeyUp = event "onKeyUp"

foreign import data FocusEvent :: *

onFocus :: forall state props action. Context state props action -> (FocusEvent -> action) -> Attr action
onFocus = event "onFocus"

onBlur :: forall state props action. Context state props action -> (FocusEvent -> action) -> Attr action
onBlur = event "onBlur"

foreign import data FormEvent :: *

onChange :: forall state props action. Context state props action -> (FormEvent -> action) -> Attr action
onChange = event "onChange"

onInput :: forall state props action. Context state props action -> (FormEvent -> action) -> Attr action
onInput = event "onInput"

onSubmit :: forall state props action. Context state props action -> (FormEvent -> action) -> Attr action
onSubmit = event "onSubmit"

foreign import data MouseEvent :: *

onClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onClick = event "onClick"

onDoubleClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDoubleClick = event "onDoubleClick"

onDrag :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDrag = event "onDrag"

onDragEnd :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDragEnd = event "onDragEnd"

onDragEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDragEnter = event "onDragEnter"

onDragExit :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDragExit = event "onDragExit"

onDragLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDragLeave = event "onDragLeave"

onDragOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDragOver = event "onDragOver"

onDragStart :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDragStart = event "onDragStart"

onDrop :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onDrop = event "onDrop"

onMouseDown :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onMouseDown = event "onMouseDown"

onMouseEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onMouseEnter = event "onMouseEnter"

onMouseLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onMouseLeave = event "onMouseLeave"

onMouseMove :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onMouseMove = event "onMouseMove"

onMouseOut :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onMouseOut = event "onMouseOut"

onMouseOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onMouseOver = event "onMouseOver"

onMouseUp :: forall state props action. Context state props action -> (MouseEvent -> action) -> Attr action
onMouseUp = event "onMouseUp"

foreign import data TouchEvent :: *

onTouchCancel :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
onTouchCancel = event "onTouchCancel"

onTouchEnd :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
onTouchEnd = event "onTouchEnd"

onTouchMove :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
onTouchMove = event "onTouchMove"

onTouchStart :: forall state props action. Context state props action -> (TouchEvent -> action) -> Attr action
onTouchStart = event "onTouchStart"

foreign import data UIEvent :: *

onScroll :: forall state props action. Context state props action -> (UIEvent -> action) -> Attr action
onScroll = event "onScroll"

foreign import data WheelEvent :: *

onWheel :: forall state props action. Context state props action -> (WheelEvent -> action) -> Attr action
onWheel = event "onWheel"
