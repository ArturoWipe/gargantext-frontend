module Gargantext.Components.Bootstrap.Tabs(tabs) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap.Components (OptLeaf, optLeaf)
import Gargantext.Utils ((?))
import Reactix.DOM.HTML as H

type Props a =
  ( value     :: a
  , callback  :: a -> Effect Unit
  , list      :: Array a
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
componentName = "b-tabs"

-- | Structural molecular component to the Bootstrap <nav-tabs> + <nav-item>
-- | simplifying a lot of the available UI/UX possibilites (type, disabled
-- | tabs, etc)
-- |
-- | https://getbootstrap.com/docs/4.6/components/navs/#tabs
tabs :: forall r a.
     Show a
  => Eq a
  => OptLeaf Options (Props a) r
tabs = optLeaf componentName options cpt where
  cpt props@{ list, value, callback } _ = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        -- Bootstrap specific classNames
        , "nav nav-tabs"
        ]
    -- Render
    pure $

      H.ul
      { className } $
      flip map list \item ->

        H.li
        { className: "nav-item"
        , on: { click: \_ -> callback item }
        }
        [
          H.a
          { className: intercalate " "
              [ "nav-link"
              , value == item ? "active" $ ""
              ]
          }
          [
            H.text $ show item
          ]
        ]
