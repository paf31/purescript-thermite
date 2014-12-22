# Module Documentation

## Module Thermite

### Types

    type Action eff a = (a -> Eff eff Unit) -> Eff eff Unit

    data ComponentClass :: * -> # ! -> *

    type PerformAction state props action eff = Fn3 state props action (Action eff state)

    type Render state props action = Fn3 (Context action) state props (Html action)

    newtype Spec eff state props action where
      Spec :: SpecRecord eff state props action -> Spec eff state props action

    type SpecRecord eff state props action = { render :: Render state props action, performAction :: PerformAction state props action eff, initialState :: state }


### Values

    createClass :: forall eff state props action. Spec eff state props action -> ComponentClass props eff

    render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit


## Module Thermite.Events

### Values

    onChange :: forall action. Context action -> (String -> action) -> Prop action

    onClick :: forall action. Context action -> action -> Prop action


## Module Thermite.Html

### Types

    data Context :: * -> *

    data Html :: * -> *

    data Prop :: * -> *

    type Props action = [Prop action]


### Values

    createElement :: forall action. String -> Props action -> [Html action] -> Html action

    text :: forall action. String -> Html action


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