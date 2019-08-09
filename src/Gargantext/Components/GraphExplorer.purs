module Gargantext.Components.GraphExplorer where

import Effect.Unsafe (unsafePerformEffect)
import Gargantext.Prelude hiding (max,min)

import Control.Monad.Cont.Trans (lift)
import Data.Array (fold, length, (!!), null)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Lens (Lens', over, (%~), (.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Number as Num
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Hooks.Sigmax.Types as Sigmax
import Gargantext.Hooks.Sigmax.Sigmajs (CameraProps, SigmaNode, cameras, getCameraProps, goTo, pauseForceAtlas2, sigmaOnMouseMove)
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Legend (legend)
import Gargantext.Components.GraphExplorer.Sidebar as Sidebar
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Graph as Graph
import Gargantext.Components.Loader2 as Loader
import Gargantext.Components.Login.Types (AuthData(..), TreeId)
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Tree as Tree
import Gargantext.Config as Config
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.Graph.Tabs as GT
import Gargantext.Router (Routes(..))
import Gargantext.Types (class Optional)
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import Thermite (Render, Spec, simpleSpec, defaultPerformAction)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)
import Reactix as R
import Reactix.DOM.HTML as RH

type GraphId = Int

type Props = (
    graphId :: GraphId
  , mCurrentRoute :: Maybe Routes
  , treeId :: Maybe Int
)

spec :: Spec (Record GET.StateGlue) (Record Props) GET.Action
spec = simpleSpec GET.performAction render
 where
   render :: Render (Record GET.StateGlue) (Record Props) GET.Action
   render dispatch props state _ =
     [ R2.scuff $ specCpt dispatch state props ]

specCpt :: (GET.Action -> Effect Unit) -> Record GET.StateGlue -> Record Props -> R.Element
specCpt d stateGlue p = R.createElement el p []
  where
    el = R.hooksComponent "SpecCpt" cpt
    cpt props _children = do
      state <- GET.fromStateGlue stateGlue

      pure $ explorer state props

explorer :: Record GET.State -> Record Props -> R.Element
explorer state props = R.createElement (explorerCpt state) props []

explorerCpt :: Record GET.State -> R.Component Props
explorerCpt state = R.hooksComponent "GraphExplorer" cpt
  where
    cpt {graphId, mCurrentRoute, treeId} _ = do
      controls <- Controls.useGraphControls
      pure $
        RH.div
          { id: "graph-explorer" }
          [
            row
            [
              outer
              [ inner
                [ row1
                  [ col [ pullLeft [ Toggle.treeToggleButton controls.showTree ] ]
                  , col [ Toggle.controlsToggleButton controls.showControls ]
                  , col [ pullRight [ Toggle.sidebarToggleButton controls.showSidePanel ] ]
                  ]
                , row [ Controls.controls controls ]
                , row [ tree {mCurrentRoute, treeId} controls, graphLoader graphId controls, Sidebar.sidebar controls ]
                , row [ ]
                ]
              ]
            ]
          ]
    outer = RH.div { className: "col-md-12" }
    inner = RH.div { className: "container-fluid", style: { paddingTop: "90px" } }
    row1 = RH.div { className: "row", style: { paddingBottom: "10px", marginTop: "-24px" } }
    row = RH.div { className: "row" }
    col = RH.div { className: "col-md-4" }
    pullLeft = RH.div { className: "pull-left" }
    pullRight = RH.div { className: "pull-right" }

    tree {treeId: Nothing} _ = RH.div {} []
    tree _ {showTree: false /\ _} = RH.div {} []
    tree {mCurrentRoute, treeId: Just treeId} _ =
      RH.div { className: "col-md-2" } [ Tree.elTreeview {mCurrentRoute, root: treeId} ]

graphLoader :: GraphId -> Record Controls.Controls -> R.Element
graphLoader graphId controls = R.createElement el {} []
  where
    el = R.hooksComponent "GraphLoader" cpt
    cpt {} _children = do
      Loader.useLoader graphId getNodes $ \{loaded} ->
        loadedGraphView controls {graphId, graph: convert loaded}

type GraphProps = (
    graphId :: GraphId
  , graph :: Graph.Graph
)

loadedGraphView :: Record Controls.Controls -> Record GraphProps -> R.Element
loadedGraphView controls props = R.createElement el props []
  where
    el = R.hooksComponent "GraphView" cpt
    cpt {graphId, graph} _children = do
      pure $ RH.div { className: colSize controls }
        [
          Graph.graph {
               forceAtlas2Settings: Graph.forceAtlas2Settings
             , graph
             , sigmaSettings: Controls.controlsToSigmaSettings controls
             }
        ]
    -- TODO: this doesn't work? seems to always render "col-md-9"
    colSize {showSidePanel: (true /\ _), showTree: (true /\ _)} = "col-md-8"
    colSize {showSidePanel: (false /\ _), showTree: (false /\ _)} = "col-md-12"
    colSize _ = "col-md-10"

convert :: GET.GraphData -> Graph.Graph
convert (GET.GraphData r) = Sigmax.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn i (GET.Node n) =
      Seq.singleton
        { id    : n.id_
        , size  : toNumber n.size
        , label : n.label
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , color : GET.intColor (cDef n.attributes)
        }
      where
        cDef (GET.Cluster {clustDefault}) = clustDefault
    edges = foldMap edgeFn r.edges
    edgeFn (GET.Edge e) = Seq.singleton {id : e.id_, source : e.source, target : e.target}

defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)
    
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


getNodes :: GraphId -> Aff GET.GraphData
getNodes graphId = get $ Config.toUrl Config.Back Config.Graph $ Just graphId