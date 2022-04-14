module Gargantext.Components.Bootstrap.ProgressBar(progressBar) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Components (OptLeaf, optLeaf)
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Reactix.DOM.HTML as H

type Props   =
  ( value     :: Number
  | Options
  )

type Options =
  ( className :: String
  , variant   :: Variant
  )

options :: Record Options
options =
  { className : ""
  , variant   : Primary
  }

componentName :: String
componentName = "b-progress-bar"

bootstrapName :: String
bootstrapName = "progress"

-- | Structural Component for the Bootsrap "Progress Bar"
-- |
-- | https://getbootstrap.com/docs/4.6/components/progress/
progressBar :: forall r. OptLeaf Options Props r
progressBar = optLeaf componentName options cpt where
  cpt props _ = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        -- Bootstrap specific classNames
        , bootstrapName
        ]
    -- Render
    pure $

      H.div
      { className }
      [
        H.div
        { className: intercalate " "
            [ "progress-bar"
            , "bg-" <> show props.variant
            ]
        , style: { width: (show props.value) <> "%" }
        , role: "progress-bar"
        , "aria-valuenow": show $ props.value
        , "aria-valuemin": "0"
        , "aria-valuemax": "100"
        }
        []
      ]
