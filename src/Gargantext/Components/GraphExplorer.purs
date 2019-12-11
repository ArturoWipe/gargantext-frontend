module Gargantext.Components.GraphExplorer where

import Gargantext.Prelude hiding (max,min)

import Data.Array as A
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (null, Nullable)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd, Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import DOM.Simple.Types (Element)
import Effect.Aff (Aff)
import Math (log)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Sidebar as Sidebar
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Graph as Graph
import Gargantext.Components.Forest (forest)
import Gargantext.Ends (Frontends)
import Gargantext.Routes (SessionRoute(NodeAPI), AppRoute)
import Gargantext.Sessions (Session, Sessions, get)
import Gargantext.Types (NodeType(Graph))
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2

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

      R.useEffect' $ do
        let readData = R.readRef dataRef
        if SigmaxTypes.eqGraph readData graph then
          pure unit
        else do
          let rSigma = R.readRef controls.sigmaRef
          Sigmax.cleanupSigma rSigma "explorerCpt"
          R.setRef dataRef graph
          snd controls.selectedNodeIds $ const Set.empty
          snd controls.graphStage $ const Graph.Init

      pure $
        RH.div
          { id: "graph-explorer" }
          [ row
            [ outer
              [ inner
                [ row1
                  [ col [ pullLeft [ Toggle.treeToggleButton controls.showTree ] ]
                  , col [ Toggle.controlsToggleButton controls.showControls ]
                  , col [ pullRight [ Toggle.sidebarToggleButton controls.showSidePanel ] ]
                  ]
                , row [ Controls.controls controls ]
                , row [ tree (fst controls.showTree) {sessions, mCurrentRoute, frontends} (snd showLogin)
                      , RH.div { ref: graphRef, id: "graph-view", className: graphClassName controls, style: {height: "95%"} } []  -- graph container
                      , graphView { controls
                                  , elRef: graphRef
                                  , graphId
                                  , graph
                                  }
                      , mSidebar graph mMetaData { frontends
                                                 , session
                                                 , selectedNodeIds: controls.selectedNodeIds
                                                 , showSidePanel: fst controls.showSidePanel
                                                 }
                      ]
                , row [
                  ]
                ]
              ]
            ]
          ]

    graphClassName :: Record Controls.Controls -> String
    graphClassName {showSidePanel: (GET.Opened /\ _), showTree: (true /\ _)} = "col-md-8"
    graphClassName {showTree: (true /\ _)} = "col-md-10"
    graphClassName {showSidePanel: (GET.Opened /\ _)} = "col-md-10"
    graphClassName _ = "col-md-12"

    outer = RH.div { className: "col-md-12" }
    inner = RH.div { className: "container-fluid", style: { paddingTop: "90px" } }
    row1  = RH.div { className: "row", style: { paddingBottom: "10px", marginTop: "-24px" } }
    row   = RH.div { className: "row" }
    col       = RH.div { className: "col-md-4" }
    pullLeft  = RH.div { className: "pull-left" }
    pullRight = RH.div { className: "pull-right" }

    tree :: Boolean
         -> {sessions :: Sessions, mCurrentRoute :: AppRoute, frontends :: Frontends}
         -> R2.Setter Boolean
         -> R.Element
    tree false _ _ = RH.div { id: "tree" } []
    tree true {sessions, mCurrentRoute: route, frontends} showLogin =
      RH.div {className: "col-md-2", style: {paddingTop: "60px"}}
      [forest {sessions, route, frontends, showLogin}]

    mSidebar :: SigmaxTypes.SGraph
             -> Maybe GET.MetaData
             -> { frontends :: Frontends
                , showSidePanel :: GET.SidePanelState
                , selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
                , session :: Session }
             -> R.Element
    mSidebar _ Nothing _ = RH.div {} []
    mSidebar graph (Just metaData) {frontends, session, selectedNodeIds, showSidePanel} =
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
)

graphView :: Record GraphProps -> R.Element
--graphView sigmaRef props = R.createElement (R.memo el memoCmp) props []
graphView props = R.createElement el props []
  where
    --memoCmp props1 props2 = props1.graphId == props2.graphId
    el = R.hooksComponent "GraphView" cpt
    cpt {controls, elRef, graphId, graph} _children = do
      -- TODO Cache this?
      let transformedGraph = transformGraph controls graph

      multiSelectEnabledRef <- R.useRef $ fst controls.multiSelectEnabled

      R.useEffect' $ do
        let nodeColor {id, color} = [id, color]
        let onlySelected g = Seq.filter (\n -> Set.member n.id (fst controls.selectedNodeIds)) $ SigmaxTypes.graphNodes g
        log2 "[graphView] selectedNodeIds" $ A.fromFoldable $ fst controls.selectedNodeIds
        log2 "[graphView] transformedGraph.nodes" $ A.fromFoldable $ map nodeColor $ onlySelected transformedGraph
        log2 "[graphView] graph.nodes" $ A.fromFoldable $ map nodeColor $ onlySelected graph

        R.setRef multiSelectEnabledRef $ fst controls.multiSelectEnabled

      pure $ Graph.graph {
          elRef
        , forceAtlas2Settings: Graph.forceAtlas2Settings
        , graph
        , multiSelectEnabledRef
        , selectedNodeIds: controls.selectedNodeIds
        , sigmaRef: controls.sigmaRef
        , sigmaSettings: Graph.sigmaSettings
        , stage: controls.graphStage
        , transformedGraph
        }

convert :: GET.GraphData -> Tuple (Maybe GET.MetaData) SigmaxTypes.SGraph
convert (GET.GraphData r) = Tuple r.metaData $ SigmaxTypes.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn i (GET.Node n) =
      Seq.singleton
        { id    : n.id_
        , size  : log (toNumber n.size + 1.0)
        , hidden : false
        , label : n.label
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , color : GET.intColor (cDef n.attributes)
        }
      where
        cDef (GET.Cluster {clustDefault}) = clustDefault
    nodesMap = SigmaxTypes.nodesMap nodes
    edges = foldMap edgeFn r.edges
    edgeFn (GET.Edge e) = Seq.singleton {id : e.id_, color, size: 1.0, source : e.source, target : e.target}
      where
        color = case Map.lookup e.source nodesMap of
          Nothing   -> "#000000"
          Just node -> node.color

defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff"
                 ,"#b399df","#ffdfed","#33c8f3","#739e9a"
                 ,"#caeca3","#f6f7e5","#f9bcca","#ccb069"
                 ,"#c9ffde","#c58683","#6c9eb0","#ffd3cf"
                 ,"#ccffc7","#52a1b0","#d2ecff","#99fffe"
                 ,"#9295ae","#5ea38b","#fff0b3","#d99e68"
                 ]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `molength defrultPalette)
    
--               div [className "col-md-12", style {"padding-bottom" : "10px"}]
--             [ menu [_id "toolbar"]
--               [ ul'
--                 [
--                 --  li' [ button [className "btn btn-success btn-sm"] [text "Change Type"] ]
--                 -- ,
--                 -- , li' [ button [className "btn btn-primary btn-sm"] [text "Change Level"] ]
--                 {- ,li [style {display : "inline-block"}]
--                   [ form'
--                     [ input [_type "file"
--                             , name "file"
--                          --   , onChange (\e -> d $ SetFile (getFile e) (unsafeCoerce $ d <<< SetProgress))
--                             , className "btn btn-primary"]

--                     -- , text $ show st.readyState
--                     ]
--                   ]
--                 -}
--                 {-, li' [ input [_type "button"
--                               , className "btn btn-warning btn-sm"
--                               ,value "Run Demo"
--                             --  , onClick \_ -> d SetGraph, disabled (st.readyState /= DONE)
--                               ]
--                       ]
--                       -}
--                 {-, li'
--                   [ form'
--                     [ div [className "col-lg-2"]
--                       [
--                         div [className "input-group"]
--                         [
--                           span [className "input-group-btn"]
--                           [
--                             button [className "btn btn-primary", _type "button"]
--                             [ span [className "glyphicon glyphicon-search"] []
--                             ]
--                           ]
--                           , input [_type "text", className "form-control", placeholder "select topics"]
--                         ]
--                       ]

--                     ]
--                   ]
--                 -}
--                  li [className "col-md-1"]
--                   [ span [] [text "Selector"]
--                   , input [ _type "range"
--                           , _id "cursorSizeRange"
--                           , min "0"
--                           , max "100"
--                           , defaultValue (show st.cursorSize)
--                           , onChange \e -> d $ ChangeCursorSize (numberTargetValue e)
--                           ]
--                   ]
--                 , li [className "col-md-1"]
--                   [ span [] [text "Labels"],input [_type "range"
--                                                  , _id "labelSizeRange"
--                                                  , max "4"
--                                                  , defaultValue <<< show $ sigmaSettings ^. _labelSizeRatio
--                                                  , min "1"
--                                                  , onChange \e -> d $ ChangeLabelSize (numberTargetValue e)
--                                                  ]
--                   ]

--                 , li [className "col-md-1"]
--                   [ span [] [text "Nodes"],input [_type "range"
--                                                  , _id "nodeSizeRange"
--                                                  , max "15"
--                                                  , defaultValue <<< show $ sigmaSettings ^. _minNodeSize
--                                                  , min "5"
--                                                  , onChange \e -> d $ ChangeNodeSize (numberTargetValue e)
--                                                  ]
--                   ]
--                 {-, li [className "col-md-2"]
--                   [ span [] [text "Edges"],input [_type "range", _id "myRange", value "90"]
--                   ]
--                 -}
--                 -- , li'
--                   -- [ button [ className "btn btn-primary"
--                   --          , onClick \_ -> modCamera0 (const {x: 0.0, y: 0.0, ratio: 1.0})
--                   --          ] [text "Center"]
--                   -- ]
--                 -- , li [className "col-md-1"]
--                 --   [ span [] [text "Zoom"],input [ _type "range"
--                 --                                 , _id "cameraRatio"
--                 --                                 , max "100"
--                 --                                 , defaultValue "0"
--                 --                                 , min "0"
--                 --                                 , onChange \e -> do
--                 --                                     let ratio = (100.0 - numberTargetValue e) / 100.0pa
--                 --                                     modCamera0 (const {ratio})
--                 --                                 ]
--                 --   ]
--                 , li [className "col-md-1"]
--                   [ span [] [text "MultiNode"]
--                   , input
--                     [ _type "checkbox"
--                     , className "checkbox"
--                     -- , checked
--                     , onChange $ const $ d ToggleMultiNodeSelection
--                     ]
--                   ]
--                 , li'
--                   [ button [ className "btn btn-primary"
--                            , onClick \_ -> pauseForceAtlas2
--                            ] [text "Spatialization"]
--                   ]
--                 {-, li'
--                   [ button [className "btn btn-primary"
--                             , onClick \_ -> do
--                                              _ <- log "Hey there" -- $ show st.camera
--                                              pure unit
--                            ] [text "Save"] -- TODO: Implement Save!
--                   ]
--                 -}
--                 ]
--               ]


getNodes :: Session -> GraphId -> Aff GET.GraphData
getNodes session graphId = get session $ NodeAPI Graph (Just graphId) ""


transformGraph :: Record Controls.Controls -> SigmaxTypes.SGraph -> SigmaxTypes.SGraph
transformGraph controls graph = SigmaxTypes.Graph {nodes: newNodes, edges: newEdges}
  where
    edges = SigmaxTypes.graphEdges graph
    nodes = SigmaxTypes.graphNodes graph
    graphEdgesMap = SigmaxTypes.edgesGraphMap graph
    graphNodesMap = SigmaxTypes.nodesGraphMap graph
    selectedEdgeIds =
      Set.fromFoldable
        $ Seq.map _.id
        $ Seq.filter (\e -> Set.member e.source (fst controls.selectedNodeIds)) edges
    hasSelection = not $ Set.isEmpty (fst controls.selectedNodeIds)

    newNodes = nodeSizes <$> nodeMarked <$> nodes
    newEdges = edgeMarked <$> edges

    nodeSizes node@{ size } =
      if Range.within (fst controls.nodeSize) size then
        node
      else
        node { hidden = true }
    edgeMarked edge@{ id } = do
      let isSelected = Set.member id selectedEdgeIds
      let sourceNode = Map.lookup edge.source graphNodesMap
      case Tuple hasSelection isSelected of
        Tuple false true  -> edge { color = "#ff0000" }
        Tuple true  true  -> edge { color = (unsafePartial $ fromJust sourceNode).color }
        Tuple true  false -> edge { color = "#dddddd" }
        _                 -> edge
    nodeMarked node@{ id } =
      if Set.member id (fst controls.selectedNodeIds) then
        node { color = "#ff0000" }
      else
        node
