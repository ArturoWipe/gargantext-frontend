module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Data.Array (null)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.PhyloExplorer.Types (SelectedTerm(..), TabView(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( nodeId          :: NodeID

  , docCount        :: Int
  , foundationCount :: Int
  , periodCount     :: Int
  , termCount       :: Int
  , groupCount      :: Int
  , branchCount     :: Int

  , selectionQuery  :: T.Box (Maybe String)
  , selectedTerms   :: T.Box (Array SelectedTerm)
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
          detailsTab
          { key: (show props.nodeId) <> "-details"
          , docCount: props.docCount
          , foundationCount: props.foundationCount
          , periodCount: props.periodCount
          , termCount: props.termCount
          , groupCount: props.groupCount
          , branchCount: props.branchCount
          }
      ,
        R2.if' (tabView == SelectionTab) $
          selectionTab
          { key: (show props.nodeId) <> "-selection"
          , selectedTerms: props.selectedTerms
          , selectionQuery: props.selectionQuery
          }
      ]


------------------------------------------------------

type DetailsProps =
  ( key             :: String

  , docCount        :: Int
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
  , selectionQuery  :: T.Box (Maybe String)
  )

selectionTab :: R2.Leaf SelectionProps
selectionTab = R2.leaf selectionTabCpt

selectionTabCpt :: R.Component SelectionProps
selectionTabCpt = here.component "selectionTab" cpt where
  cpt props _ = do
    -- State
    selectedTerms'  <- R2.useLive' props.selectedTerms
    selectionQuery' <- R2.useLive' props.selectionQuery

    -- Render
    pure $

      H.div
      { className: "phylo-selection-tab" }
      [
        -- Highlighted terms
        case selectionQuery' of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.h5
                {}
                [
                  H.text "Highlighted term"
                ]
              ,
                H.ul
                { className: "list-group" }
                [
                  H.li
                  { className: "list-group-item" }
                  [
                    H.span
                    { className: "badge badge-info" }
                    [
                      H.text s
                    ]
                  ]
                ,
                  H.li
                  { className: "list-group-item" }
                  [
                    H.a
                    { href: "https://en.wikipedia.org/w/index.php?search=\""
                        <> s
                        <> "\""
                    , target: "_blank"
                    }
                    [
                      H.text "Click here for more info"
                    ]
                  ]
                ]
              ]
            ,
              H.hr
              { className: "phylo-selection-tab__delimiter" }
            ]
      ,
        -- Selected terms
        R2.if' (not null selectedTerms') $

          H.div
          { className: "phylo-selection-tab__selection" }
          [
            H.h5
            {}
            [
              H.text "Selected terms"
            ]
          ,
            H.ul
            { className: "list-group" }
            [
              H.li
              { className: "list-group-item" }
              [
                H.ul
                {} $
                flip map selectedTerms' \(SelectedTerm { label, freq }) ->

                  H.li
                  {}
                  [
                    H.a
                    { href: "#"
                    , className: "badge badge-light"
                    }
                    [
                      H.text $ label <> show freq
                    ]
                  ]
              ]
            ]
          ]
      ]
