module Gargantext.Components.GraphExplorer.Sidebar
  ( sidebar
  ) where

import Gargantext.Prelude

import Control.Parallel (parTraverse)
import Data.Array (last, mapWithIndex)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foldable as F
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.GraphExplorer.Sidebar.ContactList (contactListWrapper)
import Gargantext.Components.GraphExplorer.Sidebar.DocList (docListWrapper)
import Gargantext.Components.GraphExplorer.Sidebar.Legend as Legend
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, FrontendError(..), NodeID, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Math as Math
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar"

type Props =
  ( metaData        :: GET.MetaData
  , session         :: Session
  , frontends       :: Frontends
  )

sidebar :: R2.Leaf Props
sidebar = R2.leaf sidebarCpt

sidebarCpt :: R.Component Props
sidebarCpt = here.component "sidebar" cpt where
  cpt props _ = do
    -- States
    { sideTab
    } <- GraphStore.use

    sideTab'  <- R2.useLive' sideTab

    -- Computed
    let
      sideTabs =
        [ GET.SideTabLegend
        , GET.SideTabData
        , GET.SideTabCommunity
        ]

    -- Render
    pure $

      H.div
      { className: "graph-sidebar" }
      [
        -- Menu
        B.tabs
        { value: sideTab'
        , list: sideTabs
        , callback: flip T.write_ sideTab
        }
      ,
        case sideTab' of
          GET.SideTabLegend     -> sideTabLegend props
          GET.SideTabData       -> sideTabData props
          GET.SideTabCommunity  -> sideTabCommunity props
      ]

------------------------------------------------------------

sideTabLegend :: R2.Leaf Props
sideTabLegend = R2.leaf sideTabLegendCpt

sideTabLegendCpt :: R.Component Props
sideTabLegendCpt = here.component "sideTabLegend" cpt
  where
    cpt { metaData: GET.MetaData { legend } } _ = pure $

      H.div
      { className: "graph-sidebar__legend-tab" }
      [
        Legend.legend
        { items: Seq.fromFoldable legend }
      ,
        H.hr {}
      ,
        documentation EN
      ]

------------------------------------------------------------

sideTabData :: R2.Leaf Props
sideTabData = R2.leaf sideTabDataCpt

sideTabDataCpt :: R.Component Props
sideTabDataCpt = here.component "sideTabData" cpt where
  cpt props _ = do
    -- States
    { selectedNodeIds
    , graph
    } <- GraphStore.use

    selectedNodeIds'  <- R2.useLive' selectedNodeIds
    graph'            <- R2.useLive' graph

    -- Computed
    let
      hasSelection = not $ Set.isEmpty selectedNodeIds'

    -- Render
    pure $

      H.div
      { className: "graph-sidebar__data-tab" }
      [
        case hasSelection of

          -- No result
          false ->

            B.caveat
            {}
            [
              H.text "Select one or more nodes to get their informations"
            ]

          -- Nodes have been selected
          true ->

            R.fragment
            [
              selectedNodes $
              { nodesMap: SigmaxT.nodesGraphMap graph'
              } `Record.merge` props
            ,
              sideBarTabSeparator
            ,
              neighborhood
              {}
            ,
              sideBarTabSeparator
            ,
              docListWrapper
              { metaData: props.metaData
              }
            ]
      ]

------------------------------------------------------------

sideTabCommunity :: R2.Leaf Props
sideTabCommunity = R2.leaf sideTabCommunityCpt

sideTabCommunityCpt :: R.Component Props
sideTabCommunityCpt = here.component "sideTabCommunity" cpt where
  cpt props _ = do
    -- States
    { selectedNodeIds
    , graph
    } <- GraphStore.use

    selectedNodeIds'  <- R2.useLive' selectedNodeIds
    graph'            <- R2.useLive' graph

    -- Computed
    let
      hasSelection = not $ Set.isEmpty selectedNodeIds'

    -- Render
    pure $

      H.div
      { className: "graph-sidebar__community-tab" }
      [
        case hasSelection of

          -- No result
          false ->

            B.caveat
            {}
            [
              H.text "Select one or more nodes to get their informations"
            ]

          -- Nodes have been selection
          true ->

            R.fragment
            [
              selectedNodes $
              { nodesMap: SigmaxT.nodesGraphMap graph'
              } `Record.merge` props
            ,
              sideBarTabSeparator
            ,
              neighborhood
              {}
            ,
              sideBarTabSeparator
            ,
              contactListWrapper
              { metaData: props.metaData
              }
            ]
      ]

-------------------------------------------

sideBarTabSeparator :: R.Element
sideBarTabSeparator =
  H.div
  { className: "graph-sidebar__separator" }
  [
    B.icon
    { name: "angle-down" }
  ]

-------------------------------------------
-- TODO
-- selectedNodes :: Record Props -> Map.Map String Nodes -> R.Element

type SelectedNodesProps =
  ( nodesMap :: SigmaxT.NodesMap
  | Props
  )

selectedNodes :: R2.Leaf SelectedNodesProps
selectedNodes = R2.leaf selectedNodesCpt

selectedNodesCpt :: R.Component SelectedNodesProps
selectedNodesCpt = here.component "selectedNodes" cpt where
  cpt props _ = do
    -- States
    { selectedNodeIds
    , graph
    } <- GraphStore.use

    selectedNodeIds' <- R2.useLive' selectedNodeIds
    graph'           <- R2.useLive' graph

    -- Behaviors
    let
      onBadgeClick id _ = T.write_ (Set.singleton id) selectedNodeIds

    -- Render
    pure $

      H.ul
      { className: intercalate " "
          [ "graph-selected-nodes"
          , "list-group"
          ]
        }
      [
        H.li
        { className: "list-group-item" }
        [
          H.ul
          {} $

          Seq.toUnfoldable $
            flip Seq.map (badges graph' selectedNodeIds') \node ->

              H.li
              { className: "graph-selected-nodes__item" }
              [
                H.a
                { className: intercalate " "
                    [ "graph-selected-nodes__badge"
                    , "badge badge-info"
                    ]
                , on: { click: onBadgeClick node.id }
                }
                [ H.text node.label ]
              ]

        ]
      ,
        H.li
        { className: intercalate " "
            [ "list-group-item"
            , "graph-selected-nodes__actions"
            ]
        }
        [
          updateTermButton
          ( props `Record.merge`
            { variant: ButtonVariant Light
            -- { variant: ButtonVariant Secondary
            , rType: CandidateTerm
            }
          )
          -- @WIP: managing colors
          -- [ H.text "Move as candidate" ]
          [
            B.icon
            { name: "pencil-square"
            , className: "mr-1 candidate-term"
            }
          ,
            H.text "Move as candidate"
          ]
        ,
          updateTermButton
          ( props `Record.merge`
            { variant: ButtonVariant Light
            -- { variant: ButtonVariant Danger
            , rType: StopTerm
            }
          )
          -- @WIP: managing colors
          -- [ H.text "Move as stop" ]
          [
            B.icon
            { name: "pencil-square"
            , className: "mr-1 stop-term"
            }
          ,
            H.text "Move as stop"
          ]
        ]
      ]

---------------------------------------------------------

neighborhood :: R2.Leaf ()
neighborhood = R2.leaf neighborhoodCpt

neighborhoodCpt :: R.Memo ()
neighborhoodCpt = R.memo' $ here.component "neighborhood" cpt where
  cpt _ _ = do
    -- States
    { selectedNodeIds
    , graph
    } <- GraphStore.use

    selectedNodeIds' <-
      R2.useLive' selectedNodeIds

    graph' <-
      R2.useLive' graph

    showMore /\ showMoreBox <-
      R2.useBox' false

    termList /\ termListBox <-
      R2.useBox' []

    termCount /\ termCountBox <-
      R2.useBox' 0

    -- Computed
    let
      minSize = F.foldl Math.min 0.0 (Seq.map _.size (SigmaxT.graphNodes graph'))

      maxSize = F.foldl Math.max 0.0 (Seq.map _.size (SigmaxT.graphNodes graph'))

      maxTruncateResult = 5

      withTruncateResults = (termCount > maxTruncateResult) && (not showMore)


    -- Behaviors
    let
      onBadgeClick id _ = T.write_ (Set.singleton id) selectedNodeIds

    -- Effects
    R.useEffect1' selectedNodeIds' do
      let refreshed = neighbourBadges graph' selectedNodeIds'
      let count     = Seq.length refreshed
      let ordered   = A.sortWith (\n -> -n.size) $ Seq.toUnfoldable refreshed
      T.write_ count   termCountBox
      T.write_ ordered termListBox
      T.write_ false showMoreBox

    -- Render
    pure $

      H.ul
      { className: intercalate " "
          [ "graph-neighborhood"
          , "list-group"
          ]
      }
      [
        -- Extracted count
        H.li
        { className: "list-group-item" }
        [
          -- @XXX: Bootstrap CSS w/ one <li> deduped the list-style-type bullet
          H.div
          { className: "graph-neighborhood__counter" }
          [
            B.wad'
            [ "text-info", "d-inline" ] $
            show termCount
          ,
            H.text $ nbsp 1 <> "terms"
          ]
        ]
      ,
        -- Word cloud
        H.li
        { className: "list-group-item" }
        [
          H.ul
          {} $
          flip mapWithIndex termList \index node ->

            R2.if'
            (
               withTruncateResults == false
            || index < maxTruncateResult
            ) $
              H.li
              { className: "graph-neighborhood__badge" }
              [
                H.a
                { className: "badge badge-light"
                -- adjust font accordingly
                , style:
                    { fontSize: badgeSize
                        minSize
                        maxSize
                        node.size
                    , lineHeight: badgeSize
                        minSize
                        maxSize
                        node.size
                    }
                , on: { click: onBadgeClick node.id }
                }
                [ H.text node.label ]
              ]
        ,
          R2.if' (withTruncateResults) $

            B.button
            { variant: ButtonVariant Light
            , callback: \_ -> T.modify_ (not) showMoreBox
            , block: true
            , className: "graph-neighborhood__show-more"
            }
            [
              H.text "Show more"
            ]
        ]
      ]

---------------------------------------------------------

type UpdateTermButtonProps =
  ( variant    :: ButtonVariant
  , nodesMap   :: SigmaxT.NodesMap
  , rType      :: TermList
  | Props
  )

updateTermButton :: R2.Component UpdateTermButtonProps
updateTermButton = R2.component updateTermButtonCpt

updateTermButtonCpt :: R.Component UpdateTermButtonProps
updateTermButtonCpt = here.component "updateTermButton" cpt where
  cpt { variant
      , metaData
      , nodesMap
      , rType
      , session
      } children = do
    -- States
    { errors
    , reloadForest
    } <- AppStore.use

    { removedNodeIds
    , selectedNodeIds
    , graphId
    } <- GraphStore.use

    selectedNodeIds' <- R2.useLive' selectedNodeIds
    graphId'         <- R2.useLive' graphId

    -- Behaviors
    let
      callback _ = do
        let nodes = mapMaybe (\id -> Map.lookup id nodesMap)
                            $ Set.toUnfoldable selectedNodeIds'
        sendPatches { errors
                    , graphId: graphId'
                    , metaData: metaData
                    , nodes
                    , session: session
                    , termList: rType
                    , reloadForest
                    }
        T.write_ selectedNodeIds' removedNodeIds
        T.write_ SigmaxT.emptyNodeIds selectedNodeIds

    -- Render
    pure $

      B.button
      { variant
      , callback
      }
      children


---------------------------------------------------------

badgeSize :: Number -> Number -> Number -> String
badgeSize minSize maxSize size =
  let
    minFontSize = 10.0
    maxFontSize = 24.0
    sizeScaled = (size - minSize) / (maxSize - minSize)  -- in [0; 1] range
    scale' = Math.log (sizeScaled + 1.0) / (Math.log 2.0)  -- in [0; 1] range
    scale = minFontSize + scale' * (maxFontSize - minFontSize)

  in
    show scale <> "px"


badges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
badges graph selectedNodeIds = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

neighbourBadges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
neighbourBadges graph selectedNodeIds = SigmaxT.neighbours graph selectedNodes' where
  selectedNodes' = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

---------------------------------------------------------

type SendPatches =
  ( errors       :: T.Box (Array FrontendError)
  , graphId      :: NodeID
  , metaData     :: GET.MetaData
  , nodes        :: Array (Record SigmaxT.Node)
  , reloadForest :: T2.ReloadS
  , session      :: Session
  , termList     :: TermList
  )

sendPatches :: Record SendPatches -> Effect Unit
sendPatches { errors, metaData, nodes, reloadForest, session, termList } = do
  launchAff_ do
    patches <- (parTraverse (sendPatch termList session metaData) nodes) -- :: Aff (Array NTC.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (Left err) -> liftEffect $ do
        T.modify_ (A.cons $ FRESTError { error: err }) errors
        here.warn2 "[sendPatches] RESTError" err
      Just (Right (NTC.Versioned _patch)) -> do
        liftEffect $ T2.reload reloadForest

-- Why is this called delete node?
sendPatch :: TermList
          -> Session
          -> GET.MetaData
          -> Record SigmaxT.Node
          -> AffRESTError NTC.VersionedNgramsPatches
sendPatch termList session (GET.MetaData metaData) node = do
    eRet  <- NTC.putNgramsPatches coreParams versioned
    case eRet of
      Left err -> pure $ Left err
      Right ret -> do
        _task <- NTC.postNgramsChartsAsync coreParams  -- TODO add task
        pure $ Right ret
  where
    nodeId :: NodeID
    nodeId = unsafePartial $ fromJust $ fromString node.id

    versioned :: NTC.VersionedNgramsPatches
    versioned = NTC.Versioned {version: metaData.list.version, data: np}

    coreParams :: NTC.CoreParams ()
    coreParams = {session, nodeId, listIds: [metaData.list.listId], tabType}

    tabNgramType :: CTabNgramType
    tabNgramType = modeTabType node.gargType

    tabType :: TabType
    tabType = TabCorpus (TabNgramType tabNgramType)

    term :: NTC.NgramsTerm
    term = NTC.normNgram tabNgramType node.label

    np :: NTC.NgramsPatches
    np = NTC.singletonPatchMap term $ NTC.NgramsPatch { patch_children: mempty, patch_list }

    patch_list :: NTC.Replace TermList
    patch_list = NTC.Replace { new: termList, old: MapTerm }



------------------------------------------------------------------------

            {-, H.div { className: "col-md-12", id: "horizontal-checkbox" }
              [ H.ul {}
                [ checkbox "Pubs"
                , checkbox "Projects"
                , checkbox "Patents"
                , checkbox "Others"
                ]
              ]
              -}
--------------------------------------------------------------------------

documentation :: Lang -> R.Element
documentation _ =

    H.div
    { className: "graph-documentation" }
    [
      H.div
      { className: "graph-documentation__text-section" }
      [
        H.p
        {}
        [
          B.b_ "What is a graph? "
        ,
          H.text "Graph is a conveniant tool to explore your documents."
        ]
      ,
        H.p
        {}
        [
          H.text $

            "Nodes are terms selected in your Map List. "
          <>
            "Node size is proportional to the number of documents with the associated term. "
        ]
      ,
        H.p
        {}
        [
          H.text $

            "Edges between nodes represent proximities of terms according to a specific distance between your documents. "
          <>
            "Link strength is proportional to the strenght of terms association."
        ]
      ]
    ,
      H.div
      { className: "graph-documentation__text-section" }
      [
        H.ul
        {}
        [
          H.li
          {}
          [
            H.text $

              "Click on a node to select/unselect and get its information."
          ]
        ,
          H.li
          {}
          [
            H.text $

              "In case of multiple selection, the button unselect clears all selections. "
            <>
              "Use your mouse scroll to zoom in and out in the graph. "
          ]
        ,
          H.li
          {}
          [
            H.text $

              "Use the node filter to create a subgraph with nodes of a given size "
            <>
              "range (e.g. display only generic terms). "
          ]
        ,
          H.li
          {}
          [
            H.text $

              "Use the edge filter so create a subgraph with links in a given range (e.g. keep the strongest association)."
          ]
        ]
      ]
    ]

{-
TODO DOC
  Conditional distance between the terms X and Y is the probability to have both terms X and Y in the same textual context.
  Distributional distance between the terms X and Y is the probability to have same others terms in the same textual context as X or Y.

Global/local view:
    The 'change level' button allows to change between global view and node centered view,
    To explore the neighborhood of a selection click on the 'change level' button.
-}
