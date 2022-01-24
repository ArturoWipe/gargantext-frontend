module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Data.Array (length, mapWithIndex, null)
import Data.Foldable (intercalate)
import Data.Int (ceil)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.PhyloExplorer.Types (SelectedTerm(..), SelectionCount(..), TabView(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( nodeId                :: NodeID

  , docCount              :: Int
  , foundationCount       :: Int
  , periodCount           :: Int
  , termCount             :: Int
  , groupCount            :: Int
  , branchCount           :: Int

  , highlightedTerm       :: T.Box (Maybe String)
  , highlightedBranch     :: T.Box (Maybe String)
  , selectedTerms         :: T.Box (Array SelectedTerm)
  , selectionCount        :: T.Box (Maybe SelectionCount)
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
          , highlightedTerm: props.highlightedTerm
          , highlightedBranch: props.highlightedBranch
          , selectionCount: props.selectionCount
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
  ( key               :: String

  , selectedTerms     :: T.Box (Array SelectedTerm)
  , highlightedTerm   :: T.Box (Maybe String)
  , highlightedBranch :: T.Box (Maybe String)
  , selectionCount    :: T.Box (Maybe SelectionCount)
  )

selectionTab :: R2.Leaf SelectionProps
selectionTab = R2.leaf selectionTabCpt

selectionTabCpt :: R.Component SelectionProps
selectionTabCpt = here.component "selectionTab" cpt where
  cpt props _ = do
    -- State
    selectedTerms'      <- R2.useLive' props.selectedTerms
    highlightedTerm'    <- R2.useLive' props.highlightedTerm
    highlightedBranch'  <- R2.useLive' props.highlightedBranch
    selectionCount'     <- R2.useLive' props.selectionCount

    showMore /\ showMoreBox <- R2.useBox' false

    let
      termCount = length selectedTerms'

      maxTruncateResult = 5

      truncateResults
         = (termCount > maxTruncateResult)
        && (not showMore)

    -- Effects

    R.useEffect1' selectedTerms' $
      -- reset "show more" button to hidding mode on selected terms change
      T.write_ false showMoreBox

    -- Render
    pure $

      H.div
      { className: "phylo-selection-tab" }
      [
        -- Highlighted branch
        case highlightedBranch' of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.h5
                {}
                [
                  H.text "Highlighted branch"
                ]
              ,
                H.ul
                { className: "list-group" }
                [
                  H.li
                  { className: "list-group-item" }
                  [
                    H.span
                    { className: intercalate " "
                        [ "phylo-selection-tab__highlight__badge"
                        , "badge badge-info"
                        ]
                    }
                    [
                      H.text s
                    ]
                  ]
                ]
              ]
            ]
      ,
        -- Highlighted term
        case highlightedTerm' of
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
                    { className: intercalate " "
                        [ "phylo-selection-tab__highlight__badge"
                        , "badge badge-info"
                        ]
                    }
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
        -- Selection Results
        R2.if' (not null selectedTerms') $

          H.div
          { className: "phylo-selection-tab__selection" }
          [
            H.h5
            {}
            [
              H.text "Selection results"
            ]
          ,
            H.ul
            { className: "list-group" }
            [
              -- Selection count
              case selectionCount' of
                Nothing                     -> mempty
                Just (SelectionCount count) ->

                  H.li
                  { className: "list-group-item" }
                  [
                    H.ul
                    { className: "phylo-selection-tab__counter" }
                    [
                      detailsCount' count.termCount "terms" true
                    ,
                      detailsCount' count.groupCount "groups" false
                    ,
                      detailsCount' count.branchCount "branches" false
                    ]
                  ]
            ,
              -- Term word cloud
              H.li
              { className: "list-group-item" }
              [
                H.ul
                {} $
                flip mapWithIndex selectedTerms'
                  \index (SelectedTerm { label, ratio }) ->

                    R2.if'
                    (
                      truncateResults == false
                    || index < maxTruncateResult
                    ) $
                      H.li
                      { className: "phylo-selection-tab__selection__item"}
                      [
                        H.a
                        { className: "badge badge-light"
                        -- adjust font size according to term frequency
                        , style:
                            { fontSize: termFontSize ratio
                            , lineHeight: termFontSize ratio
                            }
                        }
                        [
                          H.text label
                        ]
                      ]
              ,
                R2.if' (truncateResults) $
                  B.button
                  { variant: ButtonVariant Light
                  , callback: \_ -> T.modify_ not showMoreBox
                  , block: true
                  , className: "phylo-selection-tab__selection__show-more"
                  }
                  [
                    H.text "Show more"
                  ]
              ]
            ]
          ]
      ,
        -- No result
        R2.if' (null selectedTerms') $

          B.caveat
          { className: "phylo-selection-tab__nil" }
          [
            H.text "No selection has been made"
          ]
      ]

termFontSize :: Number -> String
termFontSize
    = (_ * 10.0)
  >>> (_ + 14.0)
  >>> ceil
  >>> show
  >>> (_ <> "px")

detailsCount' :: Int -> String -> Boolean -> R.Element
detailsCount' value label weighty =
  H.li
  { className: "phylo-selection-tab__counter__item" }
  [
    H.span
    { className: intercalate " "
        [ "phylo-selection-tab__counter__value"
        , weighty ? "font-weight-bold" $ ""
        ]
    }
    [
      H.text $ show value
    ]
  ,
    H.span
    { className: intercalate " "
        [ "phylo-selection-tab__counter__label"
        , weighty ? "font-weight-bold" $ ""
        ]
    }
    [
      H.text $ nbsp 1 <> label
    ]
  ]
