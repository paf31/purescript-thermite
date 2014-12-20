# Module Documentation

## Module Thermite

### Types

    type Action eff a = (a -> Eff eff Unit) -> Eff eff Unit

    data ComponentClass :: # ! -> *

    newtype Spec eff state action where
      Spec :: SpecRecord eff state action -> Spec eff state action

    type SpecRecord eff state action = { render :: Context action -> state -> Html action, performAction :: state -> action -> Action eff state, setup :: Maybe action, initialState :: state }


### Values

    createClass :: forall eff state action. Spec eff state action -> ComponentClass eff

    render :: forall eff. ComponentClass eff -> Eff (dom :: DOM | eff) Unit


## Module Thermite.Html

### Types

    data Context :: * -> *

    data Html :: * -> *

    data Prop :: * -> *

    data Props :: * -> *


### Values

    createElement :: forall action. String -> Props action -> [Html action] -> Html action

    onChange :: forall action. Context action -> (String -> action) -> Prop action

    props :: forall action. [Prop action] -> Props action

    text :: forall action. String -> Html action