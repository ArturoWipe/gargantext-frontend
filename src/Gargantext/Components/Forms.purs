module Gargantext.Components.Forms where

import Record as Record
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Utils.Reactix as R2

clearfix :: R.Element
clearfix = H.div { className: "clearfix" } []

formGroup :: Array R.Element -> R.Element
formGroup = H.div { className: "form-group" }

center :: Array R.Element -> R.Element
center = H.div { className: "center" }

card :: Array R.Element -> R.Element
card = H.div { className: "card" }

cardBlock :: Array R.Element -> R.Element
cardBlock = H.div { className: "card-block" }

cardGroup :: Array R.Element -> R.Element
cardGroup = H.div { className: "card-group" }
