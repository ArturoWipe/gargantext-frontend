module Gargantext.Components.PhyloExplorer.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector, window)
import DOM.Simple.Console (log)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import FFI.Simple ((..), (.=))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.Draw (autocompleteSearch, autocompleteSubmit, changeDisplayView, drawPhylo, exportViz, highlightSource, resetView, setGlobalD3Reference, setGlobalDependencies)
import Gargantext.Components.PhyloExplorer.ToolBar (toolBar)
import Gargantext.Components.PhyloExplorer.TopBar (topBar)
import Gargantext.Components.PhyloExplorer.Types (Term, PhyloDataSet(..), Source, sortSources, DisplayView(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Graphics.D3.Base (d3)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer"

type Props =
  ( phyloDataSet :: PhyloDataSet
  , nodeId       :: NodeID
  )

layout :: R2.Component Props
layout = R.createElement layoutCpt
layoutCpt :: R.Component Props
layoutCpt = here.component "layout" cpt where
  cpt { phyloDataSet: (PhyloDataSet o)
      , nodeId
      } _ = do
    -- States
    let defaultDisplayView = HeadingMode
    let topBarPortalKey = "portal-topbar::" <> show nodeId

    mTopBarHost <- R.unsafeHooksEffect $ R2.getElementById "portal-topbar"

    isDisplayed /\ isReadyBox <- R2.useBox' false
    sources /\ sourcesBox <- R2.useBox' (mempty :: Array Source)
    -- @WIP: move value to PhyloDataSet?
    terms /\ termsBox <- R2.useBox' (mempty :: Array Term)
    isToolBarDisplayed /\ isToolBarDisplayedBox <- R2.useBox' false
    displayView /\ displayViewBox <- R2.useBox' defaultDisplayView
    isIsolineDisplayed /\ isIsolineDisplayedBox <- R2.useBox' false

    -- Behaviors
    changeViewCallback <- pure $
          flip T.write displayViewBox
      >=> changeDisplayView

    -- Effects
    R.useEffectOnce' $ do
      (sortSources >>> flip T.write_ sourcesBox) o.sources
      setGlobalD3Reference window d3
      setGlobalDependencies window (PhyloDataSet o)
      drawPhylo
        o.branches
        o.periods
        o.groups
        o.links
        o.ancestorLinks
        o.branchLinks
        o.bb
      changeDisplayView displayView
      T.write_ true isReadyBox
      -- @WIP: handling global variables
      T.write_ (window .. "terms") termsBox

    R.useEffect1' isIsolineDisplayed do
      mEl <- querySelector document ".phylo-isoline"
      case mEl of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "display") $
            isIsolineDisplayed ? "flex" $ "none"


    -- Effects
    -- @WIP (as some actions are checked by the JS resources via DOMElement
    --      UI attribute, for now we create a temporary reference)
    R.useEffect1' displayView $
      pure $ (window .= "displayView") (show displayView)

    -- Behaviors
    toggleToolBar <- pure $ const $ T.modify_ not isToolBarDisplayedBox

    -- Render
    pure $

      H.div
      { className: "phylo" }
      [
        -- Preloading spinner
        R2.if' (not isDisplayed) $
          B.spinner
          { className: "phylo__spinner" }
      ,
        -- Topbar
        R2.createPortal' mTopBarHost
        [
          R2.fragmentWithKey topBarPortalKey
          [
            R2.if' (isDisplayed) $
              topBar
              { sourceList: sources
              , sourceCallback: highlightSource window
              , autocompleteSearchCallback: autocompleteSearch terms
              , autocompleteSubmitCallback: autocompleteSubmit
              , toolBarFlag: isToolBarDisplayed
              , toolBarCallback: toggleToolBar
              }
          ]
        ]
      ,
        -- Toolbar
        R2.if' (isToolBarDisplayed) $
          toolBar
          { resetViewCallback: const resetView
          , exportCallback: const exportViz
          , displayView
          , changeViewCallback
          , isolineBox: isIsolineDisplayedBox
          }

      ,
        -- Iso Line
        H.div
        { className: "phylo-isoline"}
        [
          H.div
          { className: "phylo-isoline__bleed-space" }
          []
        ,
          H.div
          { id: "phyloIsoLine"
          , className: "phylo-isoline__content"
          }
          []
        ]
      ,
        -- Phylo Grid
        H.div
        { className: "phylo-grid" }
        [
          H.div
          { className: "phylo-grid__blueprint" }
          [
            H.div
            { className: "phylo-grid__blueprint__left" }
            []
          ,
            H.div
            { className: "phylo-grid__blueprint__center" }
            []
          ,
            H.div
            { className: "phylo-grid__blueprint__right"}
            []
          ]
        ,
          H.div
          { className: "phylo-grid__content" }
          [
            H.div
            { className: "phylo-grid__content__scape" }
            []
          ,
            H.div
            { className: "phylo-grid__content__graph" }
            []
          ]
        ]
      ]
