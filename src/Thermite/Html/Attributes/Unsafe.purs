-- | This module defines unsafe helper functions for creating HTML attributes.

module Thermite.Html.Attributes.Unsafe where

import Thermite.Types
import Thermite.Internal

innerHTML :: String -> Attr
innerHTML s = unsafeAttribute "dangerouslySetInnerHTML" { "__html": s }
