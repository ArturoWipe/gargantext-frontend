module Gargantext.Components.GraphExplorer.TopBar (topBar) where

import Gargantext.Prelude hiding (max, min)

import Data.Maybe (Maybe)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.GraphExplorer.Search (nodeSearchControl)
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Types as GT
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( sidePanelGraph :: T.Box (Maybe (Record GEST.SidePanel))
  )

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.TopBar"

topBar :: R2.Leaf Props
topBar = R2.leaf component

component :: R.Component Props
component = here.component "topBar" cpt where
  cpt { sidePanelGraph } _ = do
    -- States
    { mGraph
    , multiSelectEnabled
    , selectedNodeIds
    , showControls
    , showSidebar
    } <- GEST.focusedSidePanel sidePanelGraph

    mGraph'         <- R2.useLive' mGraph
    showControls'   <- R2.useLive' showControls
    showSidebar' <- R2.useLive' showSidebar

    -- Render
    pure $

      H.div
      { className: "graph-topbar" }
      [
        -- Toolbar toggle
        B.button
        { className: "graph-topbar__toolbar"
        , callback: \_ -> T.modify_ (not) showControls
        , variant: showControls' ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [
          H.text $ showControls' ? "Hide toolbar" $ "Show toolbar"
        ]
      ,
        -- Sidebar toggle
        B.button
        { className: "graph-topbar__sidebar"
        , callback: \_ -> T.modify_ GT.toggleSidePanelState showSidebar

        , variant: showSidebar' == GT.Opened ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [
          H.text $ showSidebar' == GT.Opened ?
            "Hide sidebar" $
            "Show sidebar"
        ]
      ,
        -- Search
        B.fromMaybe_ mGraph' \graph ->

          nodeSearchControl
          { graph
          , multiSelectEnabled
          , selectedNodeIds
          , className: "graph-topbar__search"
          }
      ]
