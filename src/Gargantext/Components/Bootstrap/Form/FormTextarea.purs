module Gargantext.Components.Bootstrap.FormTextarea (formTextarea) where

import Gargantext.Prelude

import Data.Foldable (elem, intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap.Components (OptLeaf, optLeaf)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Reactix.DOM.HTML as H
import Unsafe.Coerce (unsafeCoerce)

type Props =
  ( callback      :: String -> Effect Unit
  , value         :: String
  | Options
  )

type Options =
  ( status        :: ComponentStatus
  , className     :: String
  , placeholder   :: String
  , rows          :: Int
  )

options :: Record Options
options =
  { status        : Enabled
  , className     : ""
  , placeholder   : ""
  , rows          : 2
  }

componentName :: String
componentName = "b-form-textarea"

bootstrapName :: String
bootstrapName = "form-control"

-- | Structural Component for the Bootstrap textarea
-- |
-- | https://getbootstrap.com/docs/4.1/components/forms/
formTextarea :: forall r. OptLeaf Options Props r
formTextarea = optLeaf componentName options cpt where
  cpt props@{ callback
            , status
            , rows
            } _ = do
    -- Computed
    className <- pure $ intercalate " "
      -- provided custom className
      [ props.className
      -- BEM classNames
      , componentName
      , componentName <> "--" <> show status
      -- Bootstrap specific classNames
      , bootstrapName
      ]

    change <- pure $ onChange status callback

    -- Render
    pure $

      H.textarea
      { className
      , on: { change }
      , disabled: elem status [ Disabled ]
      , readOnly: elem status [ Idled ]
      , placeholder: props.placeholder
      , autoComplete: "off"
      , rows
      } []

-- | * Change event will effectively be triggered according to the
-- | component status props
-- | * Also directly returns the newly input value
-- | (usage not so different from `targetValue` of ReactBasic)
onChange :: forall event.
     ComponentStatus
  -> (String -> Effect Unit)
  -> event
  -> Effect Unit
onChange status callback event = do
  if   status == Enabled
  then callback $ (unsafeCoerce event).target.value
  else pure unit
