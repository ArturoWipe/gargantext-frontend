module Gargantext.Components.Bootstrap.Conditionals
  ( if', if_
  , fromMaybe_
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Reactix as R

-- | One-liner `if` simplifying render writing
-- | (best for one child)
if' :: Boolean -> R.Element -> R.Element
if' = if _ then _ else mempty

-- | One-liner `if` simplifying render writing
-- | (best for multiple children)
if_ :: Boolean -> Array (R.Element) -> R.Element
if_ pred arr = if pred then (R.fragment arr) else mempty

-- | Render a `mempty` Element if provided `Maybe` is `Nothing`
fromMaybe_ :: forall a. Maybe a -> (a -> R.Element) -> R.Element
fromMaybe_ m render = case m of
  Nothing -> mempty
  Just a  -> render a
