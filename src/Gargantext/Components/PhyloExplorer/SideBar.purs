module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Data.Array (length, mapWithIndex, null)
import Data.Foldable (intercalate)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.PhyloExplorer.Types (ExtractedTerm(..), ExtractedCount(..), TabView(..))
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

  , selectedTerm          :: Maybe String
  , selectedBranch        :: Maybe String
  , selectedSource        :: Maybe String
  , extractedTerms        :: Array ExtractedTerm
  , extractedCount        :: Maybe ExtractedCount
  , selectTermCallback    :: String -> Effect Unit
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
        -- Teasers
        H.div
        { className: "phylo-sidebar__top-teaser" }
        []
      ,
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
        -- Details tab
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
        -- Selection tab
        R2.if' (tabView == SelectionTab) $
          selectionTab
          { key: (show props.nodeId) <> "-selection"
          , extractedTerms: props.extractedTerms
          , extractedCount: props.extractedCount
          , selectedTerm: props.selectedTerm
          , selectedBranch: props.selectedBranch
          , selectedSource: props.selectedSource
          , selectTermCallback: props.selectTermCallback
          }
      ,
        -- Teaser
        H.div
        { className: "phylo-sidebar__bottom-teaser" }
        []
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
  ( key                 :: String

  , extractedTerms      :: Array ExtractedTerm
  , extractedCount      :: Maybe ExtractedCount
  , selectedTerm        :: Maybe String
  , selectedBranch      :: Maybe String
  , selectedSource      :: Maybe String
  , selectTermCallback  :: String -> Effect Unit
  )

selectionTab :: R2.Leaf SelectionProps
selectionTab = R2.leaf selectionTabCpt

selectionTabCpt :: R.Component SelectionProps
selectionTabCpt = here.component "selectionTab" cpt where
  cpt { selectTermCallback
      , extractedTerms
      , extractedCount
      , selectedTerm
      , selectedBranch
      , selectedSource
      } _ = do

  -- State
  --------

    showMore /\ showMoreBox <- R2.useBox' false

    let
      haveSelection
         = isJust selectedTerm
        || isJust selectedBranch
        || isJust selectedSource

      termCount = length extractedTerms

      maxTruncateResult = 5

      truncateResults
         = termCount > maxTruncateResult
        && not showMore

  -- Effects
  ----------

    -- reset "show more" button to hidding mode on selected terms change
    R.useEffect1' extractedTerms $
      T.write_ false showMoreBox

  -- Render
  ---------

    pure $

      H.div
      { className: "phylo-selection-tab" }
      [
        -- No result
        R2.if' (not haveSelection) $

          B.caveat
          { className: "phylo-selection-tab__nil" }
          [
            H.text "No selection has been made"
          ]
      ,
        -- Selected source
        case selectedSource of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.h6
                {}
                [
                  H.text "Selected source"
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
        -- Selected branch
        case selectedBranch of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.h6
                {}
                [
                  H.text "Selected branch"
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
        -- Selected term
        case selectedTerm of
          Nothing -> mempty
          Just s  -> R.fragment
            [
              H.div
              { className: "phylo-selection-tab__highlight" }
              [
                H.h6
                {}
                [
                  H.text "Selected term"
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
            ]
      ,
        -- No extracted result
        R2.if' (haveSelection && null extractedTerms) $

          H.div
          { className: "phylo-selection-tab__selection" }
          [
            H.h6
            {}
            [
              H.text "Extracted result"
            ]
          ,
            B.caveat
            {}
            [
              H.text "No result found for your selection"
            ]
          ]
      ,
        -- Extracted Results
        R2.if' (not null extractedTerms) $

          H.div
          { className: "phylo-selection-tab__selection" }
          [
            H.h6
            {}
            [
              H.text "Extracted result"
            ]
          ,
            H.ul
            { className: "list-group" }
            [
              -- Extracted count
              case extractedCount of
                Nothing                     -> mempty
                Just (ExtractedCount count) ->

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
                flip mapWithIndex extractedTerms
                  \index (ExtractedTerm { label, ratio }) ->

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
                        , on:
                          { click: \_ -> selectTermCallback label
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
