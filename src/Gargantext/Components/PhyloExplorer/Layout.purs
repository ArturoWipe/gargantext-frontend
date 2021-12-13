module Gargantext.Components.PhyloExplorer.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, window)
import Data.Tuple.Nested ((/\))
import FFI.Simple ((..))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.Draw (autocompleteSearch, autocompleteSubmit, drawPhylo, highlightSource, onPhyloReady, setGlobalD3Reference, setGlobalDependencies)
import Gargantext.Components.PhyloExplorer.TopBar (topBar)
import Gargantext.Components.PhyloExplorer.Types (Term, PhyloDataSet(..), Source, sortSources)
import Gargantext.Types (NodeID)
import Gargantext.Utils (nbsp)
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
    -- @WIP
    let topBarPortalKey = "portal-topbar::" <> show nodeId

    isDisplayed /\ isReadyBox <- R2.useBox' false
    mTopBarHost <- R.unsafeHooksEffect $ R2.getElementById "portal-topbar"
    sources /\ sourcesBox <- R2.useBox' (mempty :: Array Source)
    -- @WIP: move value to PhyloDataSet?
    terms /\ termsBox <- R2.useBox' (mempty :: Array Term)

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
      onPhyloReady document o.name
      T.write_ true isReadyBox
      -- @WIP: handling global variables
      T.write_ (window .. "terms") termsBox

    -- Render
    pure $

      H.div
      { className: "phylo" }
      [
        R2.if' (not isDisplayed) $
          B.spinner
          { className: "phylo__spinner" }
      ,
        H.div
        { id: "phyloToolBar"
        , style: { visibility: "hidden" } }
        [
          phyloCorpusInfo
          { nbDocs        : o.nbDocs
          , nbFoundations : o.nbFoundations
          , nbPeriods     : o.nbPeriods
          }
        ,
          phyloHow
          {}
        ,
          phyloPhylo
          {}
        ,
          phyloPhyloInfo
          { nbTerms     : o.nbTerms
          , nbGroups    : o.nbGroups
          , nbBranches  : o.nbBranches
          }
        ]
      ,

      -- <!-- row 2 & 3 -->
        H.div
        { id: "phyloIsoLine"
        , className: "phylo-isoline"
        }
        []
      ,
        H.div
        { id: "phyloIsolineInfo"
        , className: "phylo-isoline-info"
        }
        [
          H.div
          { className: "btn-group" }
          [
            H.button
            { id: "reset"
            , className: "button reset"
            }
            [
              H.i
              { className: "fa fa-arrows-alt" }
              []
            ]
          ,
            H.button
            { id: "label"
            , className: "button label"
            }
            [
              H.i
              { className: "fa fa-dot-circle-o" }
              []
            ]
          ,
            H.button
            { id: "heading"
            , className: "button heading"
            }
            [
              H.i
              { className: "fa fa-sort-alpha-asc" }
              []
            ]
          ,
            H.button
            { id: "export"
            , className: "button export"
            }
            [
              H.i
              { className: "fas fa-camera" }
              []
            ]
          ]
        ]
      ,

      -- <!-- row 4 -->
        H.div
        { id: "phyloScape"
        , className: "phylo-scape"
        }
        []
      ,
        H.div
        { id: "phyloTimeline"
        , className: "phylo-timeline"
        }
        []
      ,
        H.div
        { id: "phyloGraph"
        , className: "phylo-graph"
        }
        []


      ,
        -- <!-- PORTAL: topbar -->
        R2.createPortal' mTopBarHost
        [
          R2.fragmentWithKey topBarPortalKey
          [
            topBar
            { sourceList: sources
            , sourceCallback: highlightSource window
            , autocompleteSearchCallback: autocompleteSearch terms
            , autocompleteSubmitCallback: autocompleteSubmit
            }
          ]
        ]
      ]

---------------------------------------------------------

phyloHow :: R2.Leaf ()
phyloHow = R2.leaf phyloHowCpt
phyloHowCpt :: R.Component ()
phyloHowCpt = here.component "phyloHow" cpt where
  cpt _ _ = do
    -- Render
    pure $

      H.div
      { id: "phyloHow"
      , className: "phylo-how"
      }
      [
        H.a
        { id: "phyloSearch"
        , href: "http://maps.gargantext.org/phylo/knowledge_visualization/memiescape/documentation.html"
        , target: "_blank"
        }
        [
          H.div
          { className: "switch" }
          [
            H.i
            { className: "far fa-question-circle how" }
            []
          ,
            H.i
            { className: "fa fa-question-circle how" }
            [
              H.span
              { className: "tooltip" }
              [ H.text "click to see how the phylomemy was built" ]
            ]
          ]
        ]
      ]

---------------------------------------------------------

phyloPhylo :: R2.Leaf ()
phyloPhylo = R2.leaf phyloPhyloCpt
phyloPhyloCpt :: R.Component ()
phyloPhyloCpt = here.component "phyloPhylo" cpt where
  cpt _ _ = do
    -- Render
    pure $

      H.div
      { id: "phyloPhylo"
      , className: "phylo-phylo"
      }
      [ H.text "phylomemy" ]


---------------------------------------------------------

type PhyloCorpusInfoProps =
  ( nbDocs        :: Int
  , nbFoundations :: Int
  , nbPeriods     :: Int
  )

phyloCorpusInfo :: R2.Leaf PhyloCorpusInfoProps
phyloCorpusInfo = R2.leaf phyloCorpusInfoCpt
phyloCorpusInfoCpt :: R.Component PhyloCorpusInfoProps
phyloCorpusInfoCpt = here.component "phyloCorpusInfo" cpt where
  cpt props _ = do
    -- Render
    pure $

      H.div
      { id: "phyloCorpusInfo"
      , className: "phylo-corpus-info"
      }
      [
        H.span
        {}
        [
          H.b {} [ H.text $ show props.nbDocs ]
        , H.text $ nbsp 1 <> "docs"
        ]
      ,
        H.span
        {}
        [
          H.b {} [ H.text $ show props.nbFoundations ]
        , H.text $ nbsp 1 <> "foundations"
        ]
      ,
        H.span
        {}
        [
          H.b {} [ H.text $ show props.nbPeriods ]
        , H.text $ nbsp 1 <> "periods"
        ]
      ]


---------------------------------------------------------

type PhyloPhyloInfoProps =
  ( nbTerms     :: Int
  , nbGroups    :: Int
  , nbBranches  :: Int
  )

phyloPhyloInfo :: R2.Leaf PhyloPhyloInfoProps
phyloPhyloInfo = R2.leaf phyloPhyloInfoCpt
phyloPhyloInfoCpt :: R.Component PhyloPhyloInfoProps
phyloPhyloInfoCpt = here.component "phyloPhyloInfo" cpt where
  cpt props _ = do
    -- Render
    pure $

      H.div
      { id: "phyloPhyloInfo"
      , className: "phylo-phylo-info"
      }
      [
        H.span
        {}
        [
          H.b
          { id: "phyloTerms" }
          [ H.text $ show props.nbTerms ]
        , H.text $ nbsp 1 <> "terms"
        ]
      ,
        H.span
        {}
        [
          H.b
          { id: "phyloGroups" }
          [ H.text $ show props.nbGroups ]
        , H.text $ nbsp 1 <> "groups"
        ]
      ,
        H.span
        {}
        [
          H.b
          { id: "phyloBranches" }
          [ H.text $ show props.nbBranches ]
        , H.text $ nbsp 1 <> "branches"
        ]
      ]
