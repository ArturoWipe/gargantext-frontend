module Gargantext.Components.Charts.Options.Legend
       (
         LegendType(..),
         PlainOrScroll(..),
         legendType,
         Orient(),
         Orientation(..),
         orient,
         SelectedMode(),
         LegendMode(..),
         selectedMode
       ) where

import Prelude (class Show, show, (<<<))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic
import Data.String (toLower)
import Unsafe.Coerce (unsafeCoerce)

newtype LegendType = LegendType String

data PlainOrScroll = Plain | Scroll
instance Show PlainOrScroll where
  show (Plain) = "plain"
  show (Scroll) = "scroll"

legendType :: PlainOrScroll -> LegendType
legendType = LegendType <<< toLower <<< show


newtype Orient = Orient String

data Orientation = Horizontal | Vertical
derive instance Generic Orientation _

orient :: Orientation -> Orient
orient = Orient <<< toLower <<< genericShow


foreign import data SelectedMode :: Type

data LegendMode = Bool Boolean | Single | Multiple
derive instance Generic LegendMode _

selectedMode :: LegendMode -> SelectedMode
selectedMode (Bool b) = unsafeCoerce b
selectedMode (Single) = unsafeCoerce "single"
selectedMode (Multiple) = unsafeCoerce "multiple"
