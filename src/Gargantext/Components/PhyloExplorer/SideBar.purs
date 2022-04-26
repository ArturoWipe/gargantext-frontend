module Gargantext.Components.PhyloExplorer.SideBar
  ( sideBar
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.DetailsTab (detailsTab)
import Gargantext.Components.PhyloExplorer.SelectionTab (selectionTab)
import Gargantext.Components.PhyloExplorer.Types (ExtractedCount, ExtractedTerm, TabView(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( phyloId               :: NodeID

  , docCount              :: Int
  , foundationCount       :: Int
  , periodCount           :: Int
  , termCount             :: Int
  , groupCount            :: Int
  , branchCount           :: Int

  , sideBarTabView        :: T.Box TabView

  , selectedTerm          :: Maybe String
  , selectedBranch        :: Maybe String
  , selectedSource        :: Maybe String
  , extractedTerms        :: Array ExtractedTerm
  , extractedCount        :: Maybe ExtractedCount
  , selectTermCallback    :: String -> Effect Unit
  )

sideBar :: R2.Leaf Props
sideBar = R2.leaf component

componentName :: String
componentName = "Gargantext.Components.PhyloExplorer.SideBar"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props _ = do
    -- States
    sideBarTabView' <- R2.useLive' props.sideBarTabView

    -- Computed
    let
      tabList = [ DetailsTab, SelectionTab ]

    -- Render
    pure $

      H.div
      { className: "phylo-sidebar" }
      [
        -- Menu
        B.tabs
        { value: sideBarTabView'
        , list: tabList
        , callback: flip T.write_ props.sideBarTabView
        }
      ,
        -- Content
        case sideBarTabView' of

          DetailsTab ->
            detailsTab
            { key: (show props.phyloId) <> "-details"
            , docCount: props.docCount
            , foundationCount: props.foundationCount
            , periodCount: props.periodCount
            , termCount: props.termCount
            , groupCount: props.groupCount
            , branchCount: props.branchCount
            }

          SelectionTab ->
            selectionTab
            { key: (show props.phyloId) <> "-selection"
            , extractedTerms: props.extractedTerms
            , extractedCount: props.extractedCount
            , selectedTerm: props.selectedTerm
            , selectedBranch: props.selectedBranch
            , selectedSource: props.selectedSource
            , selectTermCallback: props.selectTermCallback
            }
      ]
