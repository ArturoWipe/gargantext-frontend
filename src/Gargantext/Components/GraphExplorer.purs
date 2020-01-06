module Gargantext.Components.GraphExplorer where

import Gargantext.Prelude hiding (max,min)

import DOM.Simple.Types (Element)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (null, Nullable)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd, Tuple(..))
import Effect.Aff (Aff)
import Gargantext.Components.Forest (forest)
import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Sidebar as Sidebar
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Data.Louvain as Louvain
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Routes (SessionRoute(NodeAPI), AppRoute)
import Gargantext.Sessions (Session, Sessions, get)
import Gargantext.Types as Types
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Math (log)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH

type GraphId = Int

type LayoutProps =
  ( graphId :: GraphId
  , frontends :: Frontends
  , mCurrentRoute :: AppRoute
  , session :: Session
  , sessions :: Sessions
  , showLogin :: R.State Boolean
  )

type Props = (
    graph :: SigmaxTypes.SGraph
  , mMetaData :: Maybe GET.MetaData
  | LayoutProps
  )

--------------------------------------------------------------
explorerLayout :: Record LayoutProps -> R.Element
explorerLayout props = R.createElement explorerLayoutCpt props []

explorerLayoutCpt :: R.Component LayoutProps
explorerLayoutCpt = R.hooksComponent "G.C.GraphExplorer.explorerLayout" cpt
  where
    cpt {graphId, mCurrentRoute, session, sessions, frontends, showLogin} _ = do
      useLoader graphId (getNodes session) handler
      where
        handler loaded =
          explorer { graphId, mCurrentRoute, mMetaData
                   , session, sessions, graph, frontends, showLogin}
          where (Tuple mMetaData graph) = convert loaded

--------------------------------------------------------------
explorer :: Record Props -> R.Element
explorer props = R.createElement explorerCpt props []

explorerCpt :: R.Component Props
explorerCpt = R.hooksComponent "G.C.GraphExplorer.explorer" cpt
  where
    cpt {frontends, graph, graphId, mCurrentRoute, mMetaData, session, sessions, showLogin} _ = do
      dataRef <- R.useRef graph
      graphRef <- R.useRef null
      controls <- Controls.useGraphControls graph
      multiSelectEnabledRef <- R.useRef $ fst controls.multiSelectEnabled

      R.useEffect' $ do
        let readData = R.readRef dataRef
        if SigmaxTypes.eqGraph readData graph then
          pure unit
        else do
          -- Graph data changed, reinitialize sigma.
          let rSigma = R.readRef controls.sigmaRef
          Sigmax.cleanupSigma rSigma "explorerCpt"
          R.setRef dataRef graph
          -- Reinitialize bunch of state as well.
          snd controls.selectedNodeIds $ const Set.empty
          snd controls.showEdges $ const SigmaxTypes.EShow
          snd controls.forceAtlasState $ const SigmaxTypes.InitialRunning
          snd controls.graphStage $ const Graph.Init
          snd controls.showSidePanel $ const GET.InitialClosed

      pure $
        RH.div
          { id: "graph-explorer" }
          [ R2.row
            [ outer
              [ inner
                [ row1
                  [ col [ pullLeft [ Toggle.treeToggleButton controls.showTree ] ]
                  , col [ Toggle.controlsToggleButton controls.showControls ]
                  , col [ pullRight [ Toggle.sidebarToggleButton controls.showSidePanel ] ]
                  ]
                , rowControls [ Controls.controls controls ]
                , R2.row [
                        tree (fst controls.showTree) {sessions, mCurrentRoute, frontends} (snd showLogin)
                      , RH.div { ref: graphRef, id: "graph-view", className: "col-md-12" } []  -- graph container
                      , graphView { controls
                                  , elRef: graphRef
                                  , graphId
                                  , graph
                                  , multiSelectEnabledRef
                                  }
                      , mSidebar mMetaData { frontends
                                           , graph
                                           , session
                                           , selectedNodeIds: controls.selectedNodeIds
                                           , showSidePanel: fst controls.showSidePanel
                                           }
                      ]
                ]
              ]
            ]
          ]

    outer = RH.div { className: "col-md-12" }
    inner = RH.div { className: "container-fluid", style: { paddingTop: "90px" } }
    row1  = RH.div { className: "row", style: { paddingBottom: "10px", marginTop: "-24px" } }
    rowControls = RH.div { className: "row controls" }
    col       = RH.div { className: "col-md-4" }
    pullLeft  = RH.div { className: "pull-left" }
    pullRight = RH.div { className: "pull-right" }

    tree :: Boolean
         -> {sessions :: Sessions, mCurrentRoute :: AppRoute, frontends :: Frontends}
         -> R2.Setter Boolean
         -> R.Element
    tree false _ _ = RH.div { id: "tree" } []
    tree true {sessions, mCurrentRoute: route, frontends} showLogin =
      RH.div {className: "col-md-2 graph-tree"} [forest {sessions, route, frontends, showLogin}]

    mSidebar :: Maybe GET.MetaData
             -> { frontends :: Frontends
                , graph :: SigmaxTypes.SGraph
                , showSidePanel :: GET.SidePanelState
                , selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
                , session :: Session }
             -> R.Element
    mSidebar Nothing _ = RH.div {} []
    mSidebar (Just metaData) {frontends, graph, session, selectedNodeIds, showSidePanel} =
      Sidebar.sidebar { frontends
                      , graph
                      , metaData
                      , session
                      , selectedNodeIds
                      , showSidePanel
                      }

type GraphProps = (
    controls :: Record Controls.Controls
  , elRef :: R.Ref (Nullable Element)
  , graphId :: GraphId
  , graph :: SigmaxTypes.SGraph
  , multiSelectEnabledRef :: R.Ref Boolean
)

graphView :: Record GraphProps -> R.Element
--graphView sigmaRef props = R.createElement (R.memo el memoCmp) props []
graphView props = R.createElement graphViewCpt props []

graphViewCpt :: R.Component GraphProps
graphViewCpt = R.hooksComponent "GraphView" cpt
  where
    cpt {controls, elRef, graphId, graph, multiSelectEnabledRef} _children = do
      -- TODO Cache this?
      let louvainGraph =
            if (fst controls.showLouvain) then
              let louvain = Louvain.louvain unit in
              let cluster = Louvain.init louvain (SigmaxTypes.louvainNodes graph) (SigmaxTypes.louvainEdges graph) in
              SigmaxTypes.louvainGraph graph cluster
            else
              graph
      let transformedGraph = transformGraph controls louvainGraph

      R.useEffect1' (fst controls.multiSelectEnabled) $ do
        R.setRef multiSelectEnabledRef $ fst controls.multiSelectEnabled

      pure $ Graph.graph {
          elRef
        , forceAtlas2Settings: Graph.forceAtlas2Settings
        , graph
        , multiSelectEnabledRef
        , selectedNodeIds: controls.selectedNodeIds
        , showEdges: controls.showEdges
        , sigmaRef: controls.sigmaRef
        , sigmaSettings: Graph.sigmaSettings
        , stage: controls.graphStage
        , transformedGraph
        }

convert :: GET.GraphData -> Tuple (Maybe GET.MetaData) SigmaxTypes.SGraph
convert (GET.GraphData r) = Tuple r.metaData $ SigmaxTypes.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn _i (GET.Node n) =
      Seq.singleton
        { borderColor: color
        , color : color
        , equilateral: { numPoints: 3 }
        , gargType
        , hidden : false
        , id    : n.id_
        , label : n.label
        , size  : log (toNumber n.size + 1.0)
        , type  : modeGraphType gargType
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        }
      where
        cDef (GET.Cluster {clustDefault}) = clustDefault
        color = GET.intColor (cDef n.attributes)
        gargType =  unsafePartial $ fromJust $ Types.modeFromString n.type_
    nodesMap = SigmaxTypes.nodesMap nodes
    edges = foldMap edgeFn r.edges
    edgeFn (GET.Edge e) = Seq.singleton { id : e.id_
                                        , color
                                        , confluence : e.confluence
                                        , hidden : false
                                        , size: 1.0
                                        , source : e.source
                                        , sourceNode
                                        , target : e.target
                                        , targetNode
                                        , weight : e.weight }
      where
        sourceNode = unsafePartial $ fromJust $ Map.lookup e.source nodesMap
        targetNode = unsafePartial $ fromJust $ Map.lookup e.target nodesMap
        color = sourceNode.color

-- | See sigmajs/plugins/sigma.renderers.customShapes/shape-library.js
modeGraphType :: Types.Mode -> String
modeGraphType Types.Authors = "square"
modeGraphType Types.Institutes = "equilateral"
modeGraphType Types.Sources = "star"
modeGraphType Types.Terms = "def"


getNodes :: Session -> GraphId -> Aff GET.GraphData
getNodes session graphId = get session $ NodeAPI Types.Graph (Just graphId) ""


transformGraph :: Record Controls.Controls -> SigmaxTypes.SGraph -> SigmaxTypes.SGraph
transformGraph controls graph = SigmaxTypes.Graph {nodes: newNodes, edges: newEdges}
  where
    edges = SigmaxTypes.graphEdges graph
    nodes = SigmaxTypes.graphNodes graph
    selectedEdgeIds =
      Set.fromFoldable
        $ Seq.map _.id
        $ SigmaxTypes.neighbouringEdges graph (fst controls.selectedNodeIds)
    hasSelection = not $ Set.isEmpty (fst controls.selectedNodeIds)

    --newNodes = Seq.map (nodeSizeFilter <<< nodeMarked) nodes
    --newEdges = Seq.map (edgeConfluenceFilter <<< edgeWeightFilter <<< edgeShowFilter <<< edgeMarked) edges
    newEdges' = Seq.filter edgeFilter $ Seq.map (edgeShowFilter <<< edgeMarked) edges
    newNodes = Seq.filter nodeFilter $ Seq.map (nodeMarked) nodes
    newEdges = Seq.filter (edgeInGraph $ Set.fromFoldable $ Seq.map _.id newNodes) newEdges'

    edgeFilter e = edgeConfluenceFilter e &&
                   edgeWeightFilter e
                   --edgeShowFilter e
    nodeFilter n = nodeSizeFilter n

    --nodeSizeFilter node@{ size } =
    --  if Range.within (fst controls.nodeSize) size then
    --    node
    --  else
    --    node { hidden = true }
    nodeSizeFilter node@{ size } = Range.within (fst controls.nodeSize) size

    --edgeConfluenceFilter edge@{ confluence } =
    --  if Range.within (fst controls.edgeConfluence) confluence then
    --    edge
    --  else
    --    edge { hidden = true }
    edgeConfluenceFilter edge@{ confluence } = Range.within (fst controls.edgeConfluence) confluence
    edgeShowFilter edge =
      if (SigmaxTypes.edgeStateHidden $ fst controls.showEdges) then
        edge { hidden = true }
      else
        edge
    --edgeWeightFilter edge@{ weight } =
    --  if Range.within (fst controls.edgeWeight) weight then
    --    edge
    --  else
    --    edge { hidden = true }
    edgeWeightFilter :: Record SigmaxTypes.Edge -> Boolean
    edgeWeightFilter edge@{ weight } = Range.within (fst controls.edgeWeight) weight

    edgeInGraph :: SigmaxTypes.SelectedNodeIds -> Record SigmaxTypes.Edge -> Boolean
    edgeInGraph nodeIds e = (Set.member e.source nodeIds) && (Set.member e.target nodeIds)

    edgeMarked edge@{ id, sourceNode } = do
      let isSelected = Set.member id selectedEdgeIds
      case Tuple hasSelection isSelected of
        Tuple false true  -> edge { color = "#ff0000" }
        Tuple true  true  -> edge { color = sourceNode.color }
        Tuple true false  -> edge { color = "rgba(221, 221, 221, 0.5)" }
        _                 -> edge
    nodeMarked node@{ id } =
      if Set.member id (fst controls.selectedNodeIds) then
        node { borderColor = "#000", type = "selected" }
      else
        node
