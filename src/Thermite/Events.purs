module Thermite.Events where

import Thermite.Types
import Thermite.Internal

foreign import data ClipboardEvent :: *

onCopy :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action
onCopy = event "onCopy"

onCut :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action
onCut = event "onCut"

onPaste :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action
onPaste = event "onPaste"

foreign import data KeyboardEvent :: *

onKeyDown :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action
onKeyDown = event "onKeyDown"

onKeyPress :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action
onKeyPress = event "onKeyPress"

onKeyUp :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action
onKeyUp = event "onKeyUp"

foreign import data FocusEvent :: *

onFocus :: forall state props action. Context state props action -> (FocusEvent -> action) -> Prop action
onFocus = event "onFocus"

onBlur :: forall state props action. Context state props action -> (FocusEvent -> action) -> Prop action
onBlur = event "onBlur"

foreign import data FormEvent :: *

onChange :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action
onChange = event "onChange"

onInput :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action
onInput = event "onInput"

onSubmit :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action
onSubmit = event "onSubmit"

foreign import data MouseEvent :: *

onClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onClick = event "onClick"

onDoubleClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDoubleClick = event "onDoubleClick"

onDrag :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDrag = event "onDrag"

onDragEnd :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDragEnd = event "onDragEnd"

onDragEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDragEnter = event "onDragEnter"

onDragExit :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDragExit = event "onDragExit"

onDragLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDragLeave = event "onDragLeave"

onDragOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDragOver = event "onDragOver"

onDragStart :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDragStart = event "onDragStart"

onDrop :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onDrop = event "onDrop"

onMouseDown :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onMouseDown = event "onMouseDown"

onMouseEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onMouseEnter = event "onMouseEnter"

onMouseLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onMouseLeave = event "onMouseLeave"

onMouseMove :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onMouseMove = event "onMouseMove"

onMouseOut :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onMouseOut = event "onMouseOut"

onMouseOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onMouseOver = event "onMouseOver"

onMouseUp :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action
onMouseUp = event "onMouseUp"

foreign import data TouchEvent :: *

onTouchCancel :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
onTouchCancel = event "onTouchCancel"

onTouchEnd :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
onTouchEnd = event "onTouchEnd"

onTouchMove :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
onTouchMove = event "onTouchMove"

onTouchStart :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action
onTouchStart = event "onTouchStart"

foreign import data UIEvent :: *

onScroll :: forall state props action. Context state props action -> (UIEvent -> action) -> Prop action
onScroll = event "onScroll"

foreign import data WheelEvent :: *

onWheel :: forall state props action. Context state props action -> (WheelEvent -> action) -> Prop action
onWheel = event "onTouchCancel"
