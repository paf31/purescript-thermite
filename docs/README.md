# Module Documentation

## Module Thermite

### Values

    componentWillMount :: forall m state props action. action -> Spec m state props action -> Spec m state props action

    createClass :: forall eff state props action. Spec (Action eff state) state props action -> ComponentClass props eff

    render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit

    simpleSpec :: forall m state props action. state -> PerformAction props action m -> Render state props action -> Spec m state props action


## Module Thermite.Action

### Types

    data Action eff state a


### Type Class Instances

    instance applicativeAction :: Applicative (Action eff state)

    instance applyAction :: Apply (Action eff state)

    instance bindAction :: Bind (Action eff state)

    instance functorAction :: Functor (Action eff state)

    instance functorActionF :: Functor (ActionF eff state)

    instance monadAction :: Monad (Action eff state)


### Values

    async :: forall eff state a. ((a -> Eff eff Unit) -> Eff eff Unit) -> Action eff state a

    asyncSetState :: forall eff state. ((state -> Eff eff Unit) -> Eff eff Unit) -> Action eff state Unit

    getState :: forall eff state. Action eff state state

    modifyState :: forall eff state. (state -> state) -> Action eff state Unit

    runAction :: forall eff state props action a. Context state props action -> Action eff state a -> Eff eff Unit

    setState :: forall eff state. state -> Action eff state Unit

    sync :: forall eff state a. Eff eff a -> Action eff state a


## Module Thermite.Events

### Types

    data ClipboardEvent :: *

    data FocusEvent :: *

    data FormEvent :: *

    data KeyboardEvent :: *

    data MouseEvent :: *

    data TouchEvent :: *

    data UIEvent :: *

    data WheelEvent :: *


### Values

    onBlur :: forall state props action. Context state props action -> (FocusEvent -> action) -> Prop action

    onChange :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action

    onClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onCopy :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action

    onCut :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action

    onDoubleClick :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDrag :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDragEnd :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDragEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDragExit :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDragLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDragOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDragStart :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onDrop :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onFocus :: forall state props action. Context state props action -> (FocusEvent -> action) -> Prop action

    onInput :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action

    onKeyDown :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action

    onKeyPress :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action

    onKeyUp :: forall state props action. Context state props action -> (KeyboardEvent -> action) -> Prop action

    onMouseDown :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onMouseEnter :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onMouseLeave :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onMouseMove :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onMouseOut :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onMouseOver :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onMouseUp :: forall state props action. Context state props action -> (MouseEvent -> action) -> Prop action

    onPaste :: forall state props action. Context state props action -> (ClipboardEvent -> action) -> Prop action

    onScroll :: forall state props action. Context state props action -> (UIEvent -> action) -> Prop action

    onSubmit :: forall state props action. Context state props action -> (FormEvent -> action) -> Prop action

    onTouchCancel :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action

    onTouchEnd :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action

    onTouchMove :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action

    onTouchStart :: forall state props action. Context state props action -> (TouchEvent -> action) -> Prop action

    onWheel :: forall state props action. Context state props action -> (WheelEvent -> action) -> Prop action


## Module Thermite.Html

### Values

    createElement :: forall action. String -> Props action -> [Html action] -> Html action

    text :: forall action. String -> Html action


## Module Thermite.Html.Attributes

### Values

    _id :: forall action. String -> Prop action

    _type :: forall action. String -> Prop action

    accept :: forall action. String -> Prop action

    acceptCharset :: forall action. String -> Prop action

    accessKey :: forall action. String -> Prop action

    action :: forall action. String -> Prop action

    allowFullScreen :: forall action. String -> Prop action

    allowTransparency :: forall action. String -> Prop action

    alt :: forall action. String -> Prop action

    async :: forall action. String -> Prop action

    autoComplete :: forall action. String -> Prop action

    autoFocus :: forall action. Boolean -> Prop action

    autoPlay :: forall action. String -> Prop action

    cellPadding :: forall action. String -> Prop action

    cellSpacing :: forall action. String -> Prop action

    charSet :: forall action. String -> Prop action

    checked :: forall action. String -> Prop action

    classID :: forall action. String -> Prop action

    className :: forall action. String -> Prop action

    colSpan :: forall action. String -> Prop action

    cols :: forall action. String -> Prop action

    content :: forall action. String -> Prop action

    contentEditable :: forall action. String -> Prop action

    contextMenu :: forall action. String -> Prop action

    controls :: forall action. String -> Prop action

    coords :: forall action. String -> Prop action

    crossOrigin :: forall action. String -> Prop action

    dateTime :: forall action. String -> Prop action

    defer :: forall action. String -> Prop action

    dir :: forall action. String -> Prop action

    disabled :: forall action. String -> Prop action

    download :: forall action. String -> Prop action

    draggable :: forall action. String -> Prop action

    encType :: forall action. String -> Prop action

    form :: forall action. String -> Prop action

    formAction :: forall action. String -> Prop action

    formEncType :: forall action. String -> Prop action

    formMethod :: forall action. String -> Prop action

    formNoValidate :: forall action. String -> Prop action

    formTarget :: forall action. String -> Prop action

    frameBorder :: forall action. String -> Prop action

    height :: forall action. String -> Prop action

    hidden :: forall action. String -> Prop action

    href :: forall action. String -> Prop action

    hrefLang :: forall action. String -> Prop action

    htmlFor :: forall action. String -> Prop action

    httpEquiv :: forall action. String -> Prop action

    icon :: forall action. String -> Prop action

    label :: forall action. String -> Prop action

    lang :: forall action. String -> Prop action

    list :: forall action. String -> Prop action

    loop :: forall action. String -> Prop action

    manifest :: forall action. String -> Prop action

    marginHeight :: forall action. String -> Prop action

    marginWidth :: forall action. String -> Prop action

    max :: forall action. String -> Prop action

    maxLength :: forall action. String -> Prop action

    media :: forall action. String -> Prop action

    mediaGroup :: forall action. String -> Prop action

    method :: forall action. String -> Prop action

    min :: forall action. String -> Prop action

    multiple :: forall action. String -> Prop action

    muted :: forall action. String -> Prop action

    name :: forall action. String -> Prop action

    noValidate :: forall action. String -> Prop action

    open :: forall action. String -> Prop action

    pattern :: forall action. String -> Prop action

    placeholder :: forall action. String -> Prop action

    poster :: forall action. String -> Prop action

    preload :: forall action. String -> Prop action

    radioGroup :: forall action. String -> Prop action

    readOnly :: forall action. String -> Prop action

    rel :: forall action. String -> Prop action

    required :: forall action. String -> Prop action

    role :: forall action. String -> Prop action

    rowSpan :: forall action. String -> Prop action

    rows :: forall action. String -> Prop action

    sandbox :: forall action. String -> Prop action

    scope :: forall action. String -> Prop action

    scrolling :: forall action. String -> Prop action

    seamless :: forall action. String -> Prop action

    selected :: forall action. String -> Prop action

    shape :: forall action. String -> Prop action

    size :: forall action. String -> Prop action

    sizes :: forall action. String -> Prop action

    span :: forall action. String -> Prop action

    spellCheck :: forall action. String -> Prop action

    src :: forall action. String -> Prop action

    srcDoc :: forall action. String -> Prop action

    srcSet :: forall action. String -> Prop action

    start :: forall action. String -> Prop action

    step :: forall action. String -> Prop action

    tabIndex :: forall action. String -> Prop action

    target :: forall action. String -> Prop action

    title :: forall action. String -> Prop action

    useMap :: forall action. String -> Prop action

    value :: forall action. String -> Prop action

    width :: forall action. String -> Prop action

    wmode :: forall action. String -> Prop action


## Module Thermite.Html.Elements

### Values

    _data :: forall action. Props action -> [Html action] -> Html action

    _data' :: forall action. [Html action] -> Html action

    a :: forall action. Props action -> [Html action] -> Html action

    a' :: forall action. [Html action] -> Html action

    abbr :: forall action. Props action -> [Html action] -> Html action

    abbr' :: forall action. [Html action] -> Html action

    address :: forall action. Props action -> [Html action] -> Html action

    address' :: forall action. [Html action] -> Html action

    area :: forall action. Props action -> [Html action] -> Html action

    area' :: forall action. [Html action] -> Html action

    article :: forall action. Props action -> [Html action] -> Html action

    article' :: forall action. [Html action] -> Html action

    aside :: forall action. Props action -> [Html action] -> Html action

    aside' :: forall action. [Html action] -> Html action

    audio :: forall action. Props action -> [Html action] -> Html action

    audio' :: forall action. [Html action] -> Html action

    b :: forall action. Props action -> [Html action] -> Html action

    b' :: forall action. [Html action] -> Html action

    base :: forall action. Props action -> [Html action] -> Html action

    base' :: forall action. [Html action] -> Html action

    bdi :: forall action. Props action -> [Html action] -> Html action

    bdi' :: forall action. [Html action] -> Html action

    bdo :: forall action. Props action -> [Html action] -> Html action

    bdo' :: forall action. [Html action] -> Html action

    big :: forall action. Props action -> [Html action] -> Html action

    big' :: forall action. [Html action] -> Html action

    blockquote :: forall action. Props action -> [Html action] -> Html action

    blockquote' :: forall action. [Html action] -> Html action

    body :: forall action. Props action -> [Html action] -> Html action

    body' :: forall action. [Html action] -> Html action

    br :: forall action. Props action -> [Html action] -> Html action

    br' :: forall action. [Html action] -> Html action

    button :: forall action. Props action -> [Html action] -> Html action

    button' :: forall action. [Html action] -> Html action

    canvas :: forall action. Props action -> [Html action] -> Html action

    canvas' :: forall action. [Html action] -> Html action

    caption :: forall action. Props action -> [Html action] -> Html action

    caption' :: forall action. [Html action] -> Html action

    cite :: forall action. Props action -> [Html action] -> Html action

    cite' :: forall action. [Html action] -> Html action

    code :: forall action. Props action -> [Html action] -> Html action

    code' :: forall action. [Html action] -> Html action

    col :: forall action. Props action -> [Html action] -> Html action

    col' :: forall action. [Html action] -> Html action

    colgroup :: forall action. Props action -> [Html action] -> Html action

    colgroup' :: forall action. [Html action] -> Html action

    datalist :: forall action. Props action -> [Html action] -> Html action

    datalist' :: forall action. [Html action] -> Html action

    dd :: forall action. Props action -> [Html action] -> Html action

    dd' :: forall action. [Html action] -> Html action

    del :: forall action. Props action -> [Html action] -> Html action

    del' :: forall action. [Html action] -> Html action

    details :: forall action. Props action -> [Html action] -> Html action

    details' :: forall action. [Html action] -> Html action

    dfn :: forall action. Props action -> [Html action] -> Html action

    dfn' :: forall action. [Html action] -> Html action

    dialog :: forall action. Props action -> [Html action] -> Html action

    dialog' :: forall action. [Html action] -> Html action

    div :: forall action. Props action -> [Html action] -> Html action

    div' :: forall action. [Html action] -> Html action

    dl :: forall action. Props action -> [Html action] -> Html action

    dl' :: forall action. [Html action] -> Html action

    dt :: forall action. Props action -> [Html action] -> Html action

    dt' :: forall action. [Html action] -> Html action

    em :: forall action. Props action -> [Html action] -> Html action

    em' :: forall action. [Html action] -> Html action

    embed :: forall action. Props action -> [Html action] -> Html action

    embed' :: forall action. [Html action] -> Html action

    fieldset :: forall action. Props action -> [Html action] -> Html action

    fieldset' :: forall action. [Html action] -> Html action

    figcaption :: forall action. Props action -> [Html action] -> Html action

    figcaption' :: forall action. [Html action] -> Html action

    figure :: forall action. Props action -> [Html action] -> Html action

    figure' :: forall action. [Html action] -> Html action

    footer :: forall action. Props action -> [Html action] -> Html action

    footer' :: forall action. [Html action] -> Html action

    form :: forall action. Props action -> [Html action] -> Html action

    form' :: forall action. [Html action] -> Html action

    h1 :: forall action. Props action -> [Html action] -> Html action

    h1' :: forall action. [Html action] -> Html action

    h2 :: forall action. Props action -> [Html action] -> Html action

    h2' :: forall action. [Html action] -> Html action

    h3 :: forall action. Props action -> [Html action] -> Html action

    h3' :: forall action. [Html action] -> Html action

    h4 :: forall action. Props action -> [Html action] -> Html action

    h4' :: forall action. [Html action] -> Html action

    h5 :: forall action. Props action -> [Html action] -> Html action

    h5' :: forall action. [Html action] -> Html action

    h6 :: forall action. Props action -> [Html action] -> Html action

    h6' :: forall action. [Html action] -> Html action

    head :: forall action. Props action -> [Html action] -> Html action

    head' :: forall action. [Html action] -> Html action

    header :: forall action. Props action -> [Html action] -> Html action

    header' :: forall action. [Html action] -> Html action

    hr :: forall action. Props action -> [Html action] -> Html action

    hr' :: forall action. [Html action] -> Html action

    html :: forall action. Props action -> [Html action] -> Html action

    html' :: forall action. [Html action] -> Html action

    i :: forall action. Props action -> [Html action] -> Html action

    i' :: forall action. [Html action] -> Html action

    iframe :: forall action. Props action -> [Html action] -> Html action

    iframe' :: forall action. [Html action] -> Html action

    img :: forall action. Props action -> [Html action] -> Html action

    img' :: forall action. [Html action] -> Html action

    input :: forall action. Props action -> [Html action] -> Html action

    input' :: forall action. [Html action] -> Html action

    ins :: forall action. Props action -> [Html action] -> Html action

    ins' :: forall action. [Html action] -> Html action

    kbd :: forall action. Props action -> [Html action] -> Html action

    kbd' :: forall action. [Html action] -> Html action

    keygen :: forall action. Props action -> [Html action] -> Html action

    keygen' :: forall action. [Html action] -> Html action

    label :: forall action. Props action -> [Html action] -> Html action

    label' :: forall action. [Html action] -> Html action

    legend :: forall action. Props action -> [Html action] -> Html action

    legend' :: forall action. [Html action] -> Html action

    li :: forall action. Props action -> [Html action] -> Html action

    li' :: forall action. [Html action] -> Html action

    link :: forall action. Props action -> [Html action] -> Html action

    link' :: forall action. [Html action] -> Html action

    main :: forall action. Props action -> [Html action] -> Html action

    main' :: forall action. [Html action] -> Html action

    map :: forall action. Props action -> [Html action] -> Html action

    map' :: forall action. [Html action] -> Html action

    mark :: forall action. Props action -> [Html action] -> Html action

    mark' :: forall action. [Html action] -> Html action

    menu :: forall action. Props action -> [Html action] -> Html action

    menu' :: forall action. [Html action] -> Html action

    menuitem :: forall action. Props action -> [Html action] -> Html action

    menuitem' :: forall action. [Html action] -> Html action

    meta :: forall action. Props action -> [Html action] -> Html action

    meta' :: forall action. [Html action] -> Html action

    meter :: forall action. Props action -> [Html action] -> Html action

    meter' :: forall action. [Html action] -> Html action

    nav :: forall action. Props action -> [Html action] -> Html action

    nav' :: forall action. [Html action] -> Html action

    noscript :: forall action. Props action -> [Html action] -> Html action

    noscript' :: forall action. [Html action] -> Html action

    object :: forall action. Props action -> [Html action] -> Html action

    object' :: forall action. [Html action] -> Html action

    ol :: forall action. Props action -> [Html action] -> Html action

    ol' :: forall action. [Html action] -> Html action

    optgroup :: forall action. Props action -> [Html action] -> Html action

    optgroup' :: forall action. [Html action] -> Html action

    option :: forall action. Props action -> [Html action] -> Html action

    option' :: forall action. [Html action] -> Html action

    output :: forall action. Props action -> [Html action] -> Html action

    output' :: forall action. [Html action] -> Html action

    p :: forall action. Props action -> [Html action] -> Html action

    p' :: forall action. [Html action] -> Html action

    param :: forall action. Props action -> [Html action] -> Html action

    param' :: forall action. [Html action] -> Html action

    picture :: forall action. Props action -> [Html action] -> Html action

    picture' :: forall action. [Html action] -> Html action

    pre :: forall action. Props action -> [Html action] -> Html action

    pre' :: forall action. [Html action] -> Html action

    progress :: forall action. Props action -> [Html action] -> Html action

    progress' :: forall action. [Html action] -> Html action

    q :: forall action. Props action -> [Html action] -> Html action

    q' :: forall action. [Html action] -> Html action

    rp :: forall action. Props action -> [Html action] -> Html action

    rp' :: forall action. [Html action] -> Html action

    rt :: forall action. Props action -> [Html action] -> Html action

    rt' :: forall action. [Html action] -> Html action

    ruby :: forall action. Props action -> [Html action] -> Html action

    ruby' :: forall action. [Html action] -> Html action

    s :: forall action. Props action -> [Html action] -> Html action

    s' :: forall action. [Html action] -> Html action

    samp :: forall action. Props action -> [Html action] -> Html action

    samp' :: forall action. [Html action] -> Html action

    script :: forall action. Props action -> [Html action] -> Html action

    script' :: forall action. [Html action] -> Html action

    section :: forall action. Props action -> [Html action] -> Html action

    section' :: forall action. [Html action] -> Html action

    select :: forall action. Props action -> [Html action] -> Html action

    select' :: forall action. [Html action] -> Html action

    small :: forall action. Props action -> [Html action] -> Html action

    small' :: forall action. [Html action] -> Html action

    source :: forall action. Props action -> [Html action] -> Html action

    source' :: forall action. [Html action] -> Html action

    span :: forall action. Props action -> [Html action] -> Html action

    span' :: forall action. [Html action] -> Html action

    strong :: forall action. Props action -> [Html action] -> Html action

    strong' :: forall action. [Html action] -> Html action

    style :: forall action. Props action -> [Html action] -> Html action

    style' :: forall action. [Html action] -> Html action

    sub :: forall action. Props action -> [Html action] -> Html action

    sub' :: forall action. [Html action] -> Html action

    summary :: forall action. Props action -> [Html action] -> Html action

    summary' :: forall action. [Html action] -> Html action

    sup :: forall action. Props action -> [Html action] -> Html action

    sup' :: forall action. [Html action] -> Html action

    table :: forall action. Props action -> [Html action] -> Html action

    table' :: forall action. [Html action] -> Html action

    tbody :: forall action. Props action -> [Html action] -> Html action

    tbody' :: forall action. [Html action] -> Html action

    td :: forall action. Props action -> [Html action] -> Html action

    td' :: forall action. [Html action] -> Html action

    textarea :: forall action. Props action -> [Html action] -> Html action

    textarea' :: forall action. [Html action] -> Html action

    tfoot :: forall action. Props action -> [Html action] -> Html action

    tfoot' :: forall action. [Html action] -> Html action

    th :: forall action. Props action -> [Html action] -> Html action

    th' :: forall action. [Html action] -> Html action

    thead :: forall action. Props action -> [Html action] -> Html action

    thead' :: forall action. [Html action] -> Html action

    time :: forall action. Props action -> [Html action] -> Html action

    time' :: forall action. [Html action] -> Html action

    title :: forall action. Props action -> [Html action] -> Html action

    title' :: forall action. [Html action] -> Html action

    tr :: forall action. Props action -> [Html action] -> Html action

    tr' :: forall action. [Html action] -> Html action

    track :: forall action. Props action -> [Html action] -> Html action

    track' :: forall action. [Html action] -> Html action

    u :: forall action. Props action -> [Html action] -> Html action

    u' :: forall action. [Html action] -> Html action

    ul :: forall action. Props action -> [Html action] -> Html action

    ul' :: forall action. [Html action] -> Html action

    var :: forall action. Props action -> [Html action] -> Html action

    var' :: forall action. [Html action] -> Html action

    video :: forall action. Props action -> [Html action] -> Html action

    video' :: forall action. [Html action] -> Html action

    wbr :: forall action. Props action -> [Html action] -> Html action

    wbr' :: forall action. [Html action] -> Html action


## Module Thermite.Internal

### Values

    createClassImpl :: forall eff m state props action. (Context state props action -> m Unit -> Eff eff Unit) -> (forall a r. r -> (a -> r) -> Maybe a -> r) -> Spec m state props action -> ComponentClass props eff

    createElementImpl :: forall action. String -> Props action -> [Html action] -> Html action

    event :: forall state props action event. String -> Context state props action -> (event -> action) -> Prop action

    getStateImpl :: forall eff state props action. Context state props action -> Eff eff state

    renderImpl :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit

    setStateImpl :: forall eff state props action. Context state props action -> state -> Eff eff Unit

    textImpl :: forall action. String -> Html action

    unsafeAttribute :: forall action attr. String -> attr -> Prop action


## Module Thermite.Types

### Types

    data ComponentClass props (eff :: # !)

    data Context state props action

    data Html action

    type PerformAction props action m = props -> action -> m Unit

    data Prop action

    type Props action = [Prop action]

    type Render state props action = Context state props action -> state -> props -> Html action

    newtype Spec m state props action where
      Spec :: SpecRecord m state props action -> Spec m state props action

    type SpecRecord m state props action = { componentWillMount :: Maybe action, render :: Render state props action, performAction :: PerformAction props action m, initialState :: state }