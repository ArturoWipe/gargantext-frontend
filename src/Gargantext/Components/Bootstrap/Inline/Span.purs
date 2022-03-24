module Gargantext.Components.Bootstrap.Span (span', span_) where

import Reactix as R
import Reactix.DOM.HTML as H

-- | Shorthand for using HTML <span> without writing its text node
span' :: forall r. Record r -> String -> R.Element
span' props content = H.span props [ H.text content ]

-- | Shorthand for using HTML <span> without writing its text node nor props
span_ :: String -> R.Element
span_ content = H.span {} [ H.text content ]
