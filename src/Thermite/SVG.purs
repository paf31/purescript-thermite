-- | This module defines helper functions for creating SVG elements.

module Thermite.SVG where

import Thermite.Types
import Thermite.Internal
    
circle :: forall eff. Attr -> [Html eff] -> Html eff
circle = createElementImpl "circle"

clipPath :: forall eff. Attr -> [Html eff] -> Html eff
clipPath = createElementImpl "clipPath"

defs :: forall eff. Attr -> [Html eff] -> Html eff
defs = createElementImpl "defs"

ellipse :: forall eff. Attr -> [Html eff] -> Html eff
ellipse = createElementImpl "ellipse"

g :: forall eff. Attr -> [Html eff] -> Html eff
g = createElementImpl "g"

line :: forall eff. Attr -> [Html eff] -> Html eff
line = createElementImpl "line"

linearGradient :: forall eff. Attr -> [Html eff] -> Html eff
linearGradient = createElementImpl "linearGradient"

mask :: forall eff. Attr -> [Html eff] -> Html eff
mask = createElementImpl "mask"

path :: forall eff. Attr -> [Html eff] -> Html eff
path = createElementImpl "path"

pattern :: forall eff. Attr -> [Html eff] -> Html eff
pattern = createElementImpl "pattern"

polygon :: forall eff. Attr -> [Html eff] -> Html eff
polygon = createElementImpl "polygon"

polyline :: forall eff. Attr -> [Html eff] -> Html eff
polyline = createElementImpl "polyline"

radialGradient :: forall eff. Attr -> [Html eff] -> Html eff
radialGradient = createElementImpl "radialGradient"

rect :: forall eff. Attr -> [Html eff] -> Html eff
rect = createElementImpl "rect"

stop :: forall eff. Attr -> [Html eff] -> Html eff
stop = createElementImpl "stop"

svg :: forall eff. Attr -> [Html eff] -> Html eff
svg = createElementImpl "svg"

text :: forall eff. Attr -> [Html eff] -> Html eff
text = createElementImpl "text"

tspan :: forall eff. Attr -> [Html eff] -> Html eff
tspan = createElementImpl "tspan"

