-- | This module defines helper functions for creating SVG attributes.

module Thermite.SVG.Attributes where
    
import Thermite.Types
import Thermite.Internal

clipPath :: String -> Attr
clipPath = unsafeAttribute "clipPath"

cx :: String -> Attr
cx = unsafeAttribute "cx"

cy :: String -> Attr
cy = unsafeAttribute "cy"

d :: String -> Attr
d = unsafeAttribute "d"

dx :: String -> Attr
dx = unsafeAttribute "dx"

dy :: String -> Attr
dy = unsafeAttribute "dy"

fill :: String -> Attr
fill = unsafeAttribute "fill"

fillOpacity :: String -> Attr
fillOpacity = unsafeAttribute "fillOpacity"

fontFamily :: String -> Attr
fontFamily = unsafeAttribute "fontFamily"

fontSize :: String -> Attr
fontSize = unsafeAttribute "fontSize"

fx :: String -> Attr
fx = unsafeAttribute "fx"

fy :: String -> Attr
fy = unsafeAttribute "fy"

gradientTransform :: String -> Attr
gradientTransform = unsafeAttribute "gradientTransform"

gradientUnits :: String -> Attr
gradientUnits = unsafeAttribute "gradientUnits"

markerEnd :: String -> Attr
markerEnd = unsafeAttribute "markerEnd"

markerMid :: String -> Attr
markerMid = unsafeAttribute "markerMid"

markerStart :: String -> Attr
markerStart = unsafeAttribute "markerStart"

offset :: String -> Attr
offset = unsafeAttribute "offset"

opacity :: String -> Attr
opacity = unsafeAttribute "opacity"

patternContentUnits :: String -> Attr
patternContentUnits = unsafeAttribute "patternContentUnits"

patternUnits :: String -> Attr
patternUnits = unsafeAttribute "patternUnits"

points :: String -> Attr
points = unsafeAttribute "points"

preserveAspectRatio :: String -> Attr
preserveAspectRatio = unsafeAttribute "preserveAspectRatio"

r :: String -> Attr
r = unsafeAttribute "r"

rx :: String -> Attr
rx = unsafeAttribute "rx"

ry :: String -> Attr
ry = unsafeAttribute "ry"

spreadMethod :: String -> Attr
spreadMethod = unsafeAttribute "spreadMethod"

stopColor :: String -> Attr
stopColor = unsafeAttribute "stopColor"

stopOpacity :: String -> Attr
stopOpacity = unsafeAttribute "stopOpacity"

stroke :: String -> Attr
stroke = unsafeAttribute "stroke"

strokeDasharray :: String -> Attr
strokeDasharray = unsafeAttribute "strokeDasharray"

strokeLinecap :: String -> Attr
strokeLinecap = unsafeAttribute "strokeLinecap"

strokeOpacity :: String -> Attr
strokeOpacity = unsafeAttribute "strokeOpacity"

strokeWidth :: String -> Attr
strokeWidth = unsafeAttribute "strokeWidth"

textAnchor :: String -> Attr
textAnchor = unsafeAttribute "textAnchor"

transform :: String -> Attr
transform = unsafeAttribute "transform"

version :: String -> Attr
version = unsafeAttribute "version"

viewBox :: String -> Attr
viewBox = unsafeAttribute "viewBox"

x1 :: String -> Attr
x1 = unsafeAttribute "x1"

x2 :: String -> Attr
x2 = unsafeAttribute "x2"

x :: String -> Attr
x = unsafeAttribute "x"

y1 :: String -> Attr
y1 = unsafeAttribute "y1"

y2 :: String -> Attr
y2 = unsafeAttribute "y2"

y :: String -> Attr
y = unsafeAttribute "y"