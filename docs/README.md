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

    component :: forall props action eff. ComponentClass props eff -> props -> [Html action] -> Html action

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

    data Props :: * -> *


### Values

    createElement :: forall action. String -> Props action -> [Html action] -> Html action

    props :: forall action. [Prop action] -> Props action

    text :: forall action. String -> Html action