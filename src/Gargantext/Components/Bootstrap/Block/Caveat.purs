module Gargantext.Components.Bootstrap.Caveat(caveat) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Components (OptTree, optTree)
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Reactix.DOM.HTML as H

type Props   = ( | Options )
type Options =
  ( className :: String
  , variant   :: Variant
  )

options :: Record Options
options =
  { className : ""
  , variant   : Light
  }

componentName :: String
componentName = "b-caveat"

bootstrapName :: String
bootstrapName = "alert"

-- | Smart reference to the <alert> Bootstrap component,
-- | trimming every features regarding the alert system
-- |
-- | https://getbootstrap.com/docs/4.6/components/alerts/
caveat :: forall r. OptTree Options Props r
caveat = optTree componentName options cpt where
  cpt props children = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        -- Bootstrap specific classNames
        , bootstrapName
        , bootstrapName <> "-" <> show props.variant
        ]
    -- Render
    pure $

      H.div
      { className }
      children
