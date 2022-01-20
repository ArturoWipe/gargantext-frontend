module Gargantext.Components.PhyloExplorer.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector, window)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Tuple.Nested ((/\))
import FFI.Simple ((..), (.=))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.Resources (PubSubEvent(..))
import Gargantext.Components.PhyloExplorer.Resources as RS
import Gargantext.Components.PhyloExplorer.SideBar (sideBar)
import Gargantext.Components.PhyloExplorer.ToolBar (toolBar)
import Gargantext.Components.PhyloExplorer.TopBar (topBar)
import Gargantext.Components.PhyloExplorer.Types (Term, PhyloDataSet(..), Source, sortSources, DisplayView(..), SelectedTerm)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.UpdateEffect (useUpdateEffect1')
import Gargantext.Types (NodeID)
import Gargantext.Utils ((?))
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

    source  /\ sourceBox  <- R2.useBox' ""
    sources /\ sourcesBox <- R2.useBox' (mempty :: Array Source)

    -- @WIP: move value to PhyloDataSet?
    terms /\ termsBox <- R2.useBox' (mempty :: Array Term)

    toolBarDisplayed /\ toolBarDisplayedBox <- R2.useBox' false

    search /\ searchBox <- R2.useBox' ""
    result /\ resultBox <- R2.useBox' (Nothing :: Maybe Term)

    displayView /\ displayViewBox <- R2.useBox' defaultDisplayView

    isIsolineDisplayed /\ isIsolineDisplayedBox <- R2.useBox' false

    sideBarDisplayed /\ sideBarDisplayedBox <- R2.useBox' false

    selectedTermsBox <- T.useBox (mempty :: Array SelectedTerm)

    selectionQueryBox <- T.useBox (Nothing :: Maybe String)

    -- Effects
    useFirstEffect' $ do
      (sortSources >>> flip T.write_ sourcesBox) o.sources
      RS.setGlobalD3Reference window d3
      RS.setGlobalDependencies window (PhyloDataSet o)
      RS.drawPhylo
        o.branches
        o.periods
        o.groups
        o.links
        o.ancestorLinks
        o.branchLinks
        o.bb
      RS.changeDisplayView displayView
      T.write_ true isReadyBox
      -- @WIP: handling global variables
      T.write_ (window .. "terms") termsBox

    useFirstEffect' do
      -- Subscribe to new selected term change
      -- (see `Gargantext.Components.PhyloExplorer.Resources` > JavaScript >
      -- `pubsub` for detailed explanations)
      RS.subscribe (show SelectedTermsEvent) $ flip T.write_ selectedTermsBox
      -- Subscrive to new selection query change
      -- (idem)
      RS.subscribe (show SelectionQueryEvent) $ case _ of
        res
          | true == null res -> T.write_ Nothing selectionQueryBox
          | otherwise        -> T.write_ (Just res) selectionQueryBox


    R.useEffect1' isIsolineDisplayed do
      mEl <- querySelector document ".phylo-isoline"
      case mEl of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "display") $
            isIsolineDisplayed ? "flex" $ "none"

    useFirstEffect' do
      -- @WIP: remove inopinent <div> (see Gargantext.Components.Router)
      mEl <- querySelector document ".main-page__main-route .container"
      case mEl of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "display") "none"
      -- @WIP: reset "main-page__main-route" wrapper margin
      --       (see Gargantext.Components.Router)
      mEl' <- querySelector document ".main-page__main-route"
      case mEl' of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "padding") "initial"

    -- @WIP (as some actions are checked by the JS resources via DOMElement
    --      UI attribute, for now we create a temporary reference)
    useUpdateEffect1' displayView do
      pure $ (window .= "displayView") (show displayView)

    useUpdateEffect1' source do
      s <- pure $ null source ? Nothing $ Just source
      RS.highlightSource window s

    useUpdateEffect1' search do
      RS.autocompleteSearch terms search >>= flip T.write_ resultBox

    -- Behaviors
    changeViewCallback <- pure $
          flip T.write displayViewBox
      >=> RS.changeDisplayView

    autocompleteSubmitCallback <- pure $ const $
      RS.autocompleteSubmit displayViewBox result

    unselectCallback <- pure $ const do
      -- unselect sourcce
      T.write_ "" sourceBox
      -- unselect branch/term(s)
      RS.doubleClick

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
              { sources
              , source: sourceBox
              , toolBar: toolBarDisplayedBox
              , sideBar: sideBarDisplayedBox
              , result: resultBox
              , search: searchBox
              , submit: autocompleteSubmitCallback
              }
          ]
        ]
      ,
        -- Sidebar
        R2.if' (sideBarDisplayed) $
          sideBar
          { nodeId
          , docCount: o.nbDocs
          , foundationCount: o.nbFoundations
          , periodCount: o.nbPeriods
          , termCount: o.nbTerms
          , groupCount: o.nbGroups
          , branchCount: o.nbBranches
          , selectedTerms: selectedTermsBox
          , selectionQuery: selectionQueryBox
          }
      ,
        -- Toolbar
        R2.if' (toolBarDisplayed) $
          toolBar
          { resetViewCallback: const RS.resetView
          , exportCallback: const RS.exportViz
          , unselectCallback: unselectCallback
          , displayView
          , changeViewCallback
          , isolineBox: isIsolineDisplayedBox
          }
      ,
        -- Iso Line
        H.div
        { className: "phylo-isoline"}
        []
      ,
        -- (?) prefer div "margin" instead of CSS margin, it will ease
        --     some computation made on the scape and peak
        H.div
        { className: "phylo-margin" }
        []
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
      ,
        -- (?) prefer div "margin" instead of CSS margin, it will ease
        --     some computation made on the scape and peak
        H.div
        { className: "phylo-margin" }
        []
      ]
