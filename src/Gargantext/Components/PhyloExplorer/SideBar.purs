module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.PhyloExplorer.Types (SelectedTerm, TabView(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RX
import Toestand as T

type Props =
  ( nodeId          :: NodeID

  , selectedTerms   :: T.Box (Array SelectedTerm)

  | CountData
  )


here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.SideBar"

sideBar :: R2.Leaf Props
sideBar = R2.leaf component

component :: R.Component Props
component = here.component "main" cpt where
  cpt props _ = do
    -- States
    tabView /\ tabViewBox <- R2.useBox' DetailsTab

    -- Render
    pure $

      H.div
      { className: "phylo-sidebar" }
      [
        -- Menu
        H.ul
        { className: intercalate " "
            [ "nav nav-tabs"
            , "phylo-sidebar__menu"
            ]
        }
        [
          H.li
          { className: "nav-item"
          , on: { click: \_ -> T.write_ DetailsTab tabViewBox }
          }
          [
            H.a
            { className: intercalate " "
                [ "nav-link"
                , tabView == DetailsTab ? "active" $ ""
                ]
            }
            [
              H.text "Details"
            ]
          ]
        ,
          H.li
          { className: "nav-item"
          , on: { click: \_ -> T.write_ SelectionTab tabViewBox }
          }
          [
            H.a
            { className: intercalate " "
                [ "nav-link"
                , tabView == SelectionTab ? "active" $ ""
                ]
            }
            [
              H.text "Selection"
            ]
          ]
        ]
      ,
        R2.if' (tabView == DetailsTab) $
          detailsTab $ Record.merge
            { key: (show props.nodeId) <> "-details" }
            (RX.pick props :: Record CountData)
      ,
        R2.if' (tabView == SelectionTab) $
          selectionTab
          { key: (show props.nodeId) <> "-selection"
          , selectedTerms: props.selectedTerms
          }
      ]


------------------------------------------------------

type DetailsProps =
  ( key             :: String
  | CountData
  )

type CountData =
  ( docCount        :: Int
  , foundationCount :: Int
  , periodCount     :: Int
  , termCount       :: Int
  , groupCount      :: Int
  , branchCount     :: Int
  )

detailsTab :: R2.Leaf DetailsProps
detailsTab = R2.leaf detailsTabCpt

detailsTabCpt :: R.Component DetailsProps
detailsTabCpt = here.component "detailsTab" cpt where
  cpt props _ =

    -- Render
    pure $

      H.div
      { className: "phylo-details-tab" }
      [
        -- Counters
        H.ul
        { className: "phylo-details-tab__counter" }
        [
          detailsCount props.docCount "docs"
        ,
          detailsCount props.foundationCount "foundations"
        ,
          detailsCount props.periodCount "periods"
        ]
      ,
        H.ul
        { className: "phylo-details-tab__counter" }
        [
          detailsCount props.termCount "terms"
        ,
          detailsCount props.groupCount "groups"
        ,
          detailsCount props.branchCount "branches"
        ]
      ,
        H.hr
        { className: "phylo-details-tab__delimiter" }
      ,
        -- Link description
        H.a
        { className: "phylo-details-tab__link"
        , href: "http://maps.gargantext.org/unpublished_maps_phylo/vaccines_countries/documentation.html"
        , target: "_blank" }
        [
          H.text "How the phylomemy was built?"
        ]
      ]

detailsCount :: Int -> String -> R.Element
detailsCount value label =
  H.li
  { className: "phylo-details-tab__counter__item" }
  [
    H.span
    { className: "phylo-details-tab__counter__value" }
    [
      H.text $ show value
    ]
  ,
    H.span
    { className: "phylo-details-tab__counter__label "}
    [
      H.text $ nbsp 1 <> label
    ]
  ]

------------------------------------------------------

type SelectionProps =
  ( key             :: String

  , selectedTerms   :: T.Box (Array SelectedTerm)
  )

selectionTab :: R2.Leaf SelectionProps
selectionTab = R2.leaf selectionTabCpt

selectionTabCpt :: R.Component SelectionProps
selectionTabCpt = here.component "selectionTab" cpt where
  cpt props _ = do
    -- State
    selectedTerms' <- R2.useLive' props.selectedTerms

    -- Render
    pure $

      H.div
      {}
      [
        H.text $ show selectedTerms'
      ]
