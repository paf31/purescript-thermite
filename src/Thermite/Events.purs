module Thermite.Events where

import Thermite
import Thermite.Html

foreign import event """
  function event(name) {
    return function onClick(context) {
      return function(f) {
        return [name, function(e) {
          context.performAction(f(e));
        }];
      };
    };
  }
  """ :: forall action event. String -> Context action -> (event -> action) -> Prop action

foreign import data ClipboardEvent :: *

onCopy :: forall action. Context action -> (ClipboardEvent -> action) -> Prop action
onCopy = event "onCopy"

onCut :: forall action. Context action -> (ClipboardEvent -> action) -> Prop action
onCut = event "onCut"

onPaste :: forall action. Context action -> (ClipboardEvent -> action) -> Prop action
onPaste = event "onPaste"

foreign import data KeyboardEvent :: *

onKeyDown :: forall action. Context action -> (KeyboardEvent -> action) -> Prop action
onKeyDown = event "onKeyDown"

onKeyPress :: forall action. Context action -> (KeyboardEvent -> action) -> Prop action
onKeyPress = event "onKeyPress"

onKeyUp :: forall action. Context action -> (KeyboardEvent -> action) -> Prop action
onKeyUp = event "onKeyUp"

foreign import data FocusEvent :: *

onFocus :: forall action. Context action -> (FocusEvent -> action) -> Prop action
onFocus = event "onFocus"

onBlur :: forall action. Context action -> (FocusEvent -> action) -> Prop action
onBlur = event "onBlur"

foreign import data FormEvent :: *

onChange :: forall action. Context action -> (FormEvent -> action) -> Prop action
onChange = event "onChange"

onInput :: forall action. Context action -> (FormEvent -> action) -> Prop action
onInput = event "onInput"

onSubmit :: forall action. Context action -> (FormEvent -> action) -> Prop action
onSubmit = event "onSubmit"

foreign import data MouseEvent :: *

onClick :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onClick = event "onClick"

onDoubleClick :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDoubleClick = event "onDoubleClick"

onDrag :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDrag = event "onDrag"

onDragEnd :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDragEnd = event "onDragEnd"

onDragEnter :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDragEnter = event "onDragEnter"

onDragExit :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDragExit = event "onDragExit"

onDragLeave :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDragLeave = event "onDragLeave"

onDragOver :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDragOver = event "onDragOver"

onDragStart :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDragStart = event "onDragStart"

onDrop :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onDrop = event "onDrop"

onMouseDown :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onMouseDown = event "onMouseDown"

onMouseEnter :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onMouseEnter = event "onMouseEnter"

onMouseLeave :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onMouseLeave = event "onMouseLeave"

onMouseMove :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onMouseMove = event "onMouseMove"

onMouseOut :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onMouseOut = event "onMouseOut"

onMouseOver :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onMouseOver = event "onMouseOver"

onMouseUp :: forall action. Context action -> (MouseEvent -> action) -> Prop action
onMouseUp = event "onMouseUp"

foreign import data TouchEvent :: *

onTouchCancel :: forall action. Context action -> (TouchEvent -> action) -> Prop action
onTouchCancel = event "onTouchCancel"

onTouchEnd :: forall action. Context action -> (TouchEvent -> action) -> Prop action
onTouchEnd = event "onTouchEnd"

onTouchMove :: forall action. Context action -> (TouchEvent -> action) -> Prop action
onTouchMove = event "onTouchMove"

onTouchStart :: forall action. Context action -> (TouchEvent -> action) -> Prop action
onTouchStart = event "onTouchStart"

foreign import data UIEvent :: *

onScroll :: forall action. Context action -> (UIEvent -> action) -> Prop action
onScroll = event "onScroll"

foreign import data WheelEvent :: *

onWheel :: forall action. Context action -> (WheelEvent -> action) -> Prop action
onWheel = event "onTouchCancel"