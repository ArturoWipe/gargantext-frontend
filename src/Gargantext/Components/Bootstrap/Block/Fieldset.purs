module Gargantext.Components.Bootstrap.Fieldset
  ( fieldset
  ) where

import Gargantext.Prelude

import Data.Array (intercalate)
import Gargantext.Components.Bootstrap.Components (OptTree, optTree)
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( titleSlot :: R.Element
  | Options
  )

type Options =
  ( className         :: String
  , contentClassName  :: String
  )

options :: Record Options
options =
  { className         : ""
  , contentClassName  : ""
  }

componentName :: String
componentName = "b-fieldset"

-- | Component simulating a native <fieldset>
-- | (which has been completly reset by Bootstrap libraries)
fieldset :: forall r. OptTree Options Props r
fieldset = optTree componentName options cpt where
  cpt props@{ titleSlot
            } children = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        ]

      contentClassName = intercalate " "
        -- provided custom className
        [ props.contentClassName
        -- BEM classNames
        , componentName <> "__content"
        ]

    -- Render
    pure $

      H.section
      { className }
      [
        H.div
        { className: componentName <> "__legend" }
        [ titleSlot ]
      ,
        H.div
        { className: contentClassName}
        children
      ]
