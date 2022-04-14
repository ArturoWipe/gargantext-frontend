module Gargantext.Components.Bootstrap.Icon (icon) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Components (OptLeaf, optLeaf)
import Reactix.DOM.HTML as H

type Props =
  ( name  :: String
  | Options
  )

type Options =
  ( className :: String
  )

options :: Record Options
options =
  { className : ""
  }

componentName :: String
componentName = "b-icon"

bootstrapName :: String
bootstrapName = "fa"

-- | Structural Component for a simple Glyphicon element
-- |
-- | https://forkaweso.me/Fork-Awesome/icons/
icon :: forall r. OptLeaf Options Props r
icon = optLeaf componentName options cpt where
  cpt props _ = do
    -- Computed
    className <- pure $ intercalate " "
      -- provided custom className
      [ props.className
      -- BEM classNames
      , componentName
      -- Bootstrap specific classNames
      , bootstrapName
      , bootstrapName <> "-" <> props.name
      ]
    -- Render
    pure $

      H.i
      { className }
      []
