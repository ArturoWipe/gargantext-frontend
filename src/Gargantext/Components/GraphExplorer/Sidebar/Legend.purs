module Gargantext.Components.GraphExplorer.Sidebar.Legend
  ( Props, legend
  ) where

import Prelude hiding (map)

import Data.Array as A
import Data.Maybe (isJust, maybe)
import Data.Sequence (Seq)
import Data.Set as Set
import Data.Traversable (foldMap, intercalate)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as ST
import Gargantext.Utils (getter, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar.Legend"

type Props =
  ( legendSeq             :: Seq GET.Legend
  , extractedNodeList     :: Array GET.Node
  , nodeCountList         :: Array GET.ClusterCount
  , selectedNodeIds       :: T.Box ST.NodeIds
  )

legend :: R2.Leaf Props
legend = R2.leaf legendCpt

legendCpt :: R.Component Props
legendCpt = here.component "legend" cpt where
  cpt { legendSeq
      , extractedNodeList
      , nodeCountList
      , selectedNodeIds
      } _ = do
    -- | Hooks
    -- |
    R.useEffectOnce' $ here.info2 "legend" extractedNodeList
    -- | Render
    -- |
    pure $

      H.ul
      { className: "graph-legend" }
      [
        flip foldMap legendSeq \(GET.Legend { id_, label }) ->

          H.li
          { className: "graph-legend__item" }
          [
            H.div
            { className: "graph-legend__code"
            , style: { backgroundColor: GET.intColor id_ }
            }
            []
          ,
            H.div
            {}
            [
              H.div
              { className: "graph-legend__title" }
              [
                B.wad'
                [ "d-inline", "text-bold", "color-gray-700" ] $
                label
              ,
                B.wad'
                [ "d-inline", "color-gray-600", "font-size-80", "float-right" ]
                (
                  show $ getClusterNodeCount nodeCountList id_
                )
              ]
            ,
              selectedNodes
              { selectedNodeIds
              , extractedNodeList
              , clusterId: id_
              }
            ]
          ]
      ]

filterByCluster :: Int -> Array GET.Node -> Array GET.Node
filterByCluster id
  =   A.filter
      (   getter _.attributes
      >>> getter _.clustDefault
      >>> eq id
      )

getClusterNodeCount :: Array GET.ClusterCount -> Int -> Int
getClusterNodeCount nodeCountList id
  =   nodeCountList
  #   A.find
      (   getter _.id
      >>> eq id
      )
  >>> maybe 0
      (   getter _.count
      )

---------------------------------------------------------

type SelectedNodesProps =
  ( extractedNodeList     :: Array GET.Node
  , selectedNodeIds       :: T.Box ST.NodeIds
  , clusterId             :: Int
  )

selectedNodes :: R2.Leaf SelectedNodesProps
selectedNodes = R2.leaf selectedNodesCpt

selectedNodesCpt :: R.Component SelectedNodesProps
selectedNodesCpt = here.component "selectedNodes" cpt where
  cpt { extractedNodeList
      , selectedNodeIds
      , clusterId
      } _ = do
    -- | States
    -- |
    selectedNodeIds' <- R2.useLive' selectedNodeIds
    -- | Computed
    -- |
    let
      isSelected id
        =   selectedNodeIds'
        # A.fromFoldable
        # A.find
            ( eq id
            )
        # isJust

    -- | Behaviors
    -- |
    let
      onBadgeClick id _ = T.write_ (Set.singleton id) selectedNodeIds

    -- | Render
    -- |
    pure $

      H.ul
      { className: "graph-legend-nodes" }
      [
        flip foldMap (filterByCluster clusterId extractedNodeList)
        \(GET.Node { label: nodeLabel, id_: nodeId }) ->

          H.li
          { className: "graph-legend-nodes__item" }
          [
            H.a
            { className: intercalate " "
                [ "graph-legend-nodes__badge"
                , "badge"
                , (isSelected nodeId) ? "badge-info" $ "badge-light"
                ]
            , on: { click: onBadgeClick nodeId }
            }
            [ H.text nodeLabel ]
          ]
      ]
