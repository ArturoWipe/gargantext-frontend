module Gargantext.Components.PhyloExplorer.ToolBar
  ( toolBar
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.PhyloExplorer.Types (Term(..), Source(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.ToolBar"

toolBar :: R2.Leaf ()
toolBar = R2.leaf component

component :: R.Component ()
component = here.component "main" cpt where
  cpt props _ = do

    -- Render
    pure $

      H.div
      { className: "phylo-toolbar" }
      [
        H.text "Hello"
      ]
