module Gargantext.Components.Nodes.Texts where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..))
import Gargantext.Components.Charts.Options.ECharts (dispatchAction)
import Gargantext.Components.Charts.Options.Type (EChartsInstance, EChartActionData)
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.DocsTable.Types (Year)
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Document as D
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Components.Nodes.Texts.Types as TT
import Gargantext.Components.Reload (textsReloadContext)
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as Table
import Gargantext.Config.REST (logRESTError)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Sessions (Session, getCacheState, sessionId)
import Gargantext.Types (CTabNgramType(..), ListId, NodeID, SidePanelState(..), TabSubType(..), TabType(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Texts"

type Props =
  ( frontends :: Frontends
  , nodeId    :: NodeID
  )

textsLayout :: R2.Leaf Props
textsLayout = R2.leaf textsLayoutCpt

textsLayoutCpt :: R.Component Props
textsLayoutCpt = here.component "textsLayout" cpt where
  cpt { frontends, nodeId } _ = do

    _ /\ reloadBox <- R2.useBox' T2.newReload

    pure $
      R.provideContext textsReloadContext (Just reloadBox)
      [
        textsLayoutWithKey
          { key: show nodeId
          , frontends
          , nodeId
          }
      ]

---------------------------------------------------------------

textsLayoutWithKey :: R2.Leaf ( key :: String | Props )
textsLayoutWithKey = R2.leaf textsLayoutWithKeyCpt

textsLayoutWithKeyCpt :: R.Component ( key :: String | Props )
textsLayoutWithKeyCpt = here.component "textsLayoutWithKey" cpt where
  cpt { frontends
      , nodeId
      } _ = do
    session <- useSession

    boxes@{ sidePanelTexts } <- AppStore.use

    cacheState <- T.useBox $ getCacheState LT.CacheOff session nodeId
    cacheState' <- T.useLive T.unequal cacheState

    yearFilter <- T.useBox (Nothing :: Maybe Year)

    eChartsInstance <- T.useBox (Nothing :: Maybe EChartsInstance)

    R.useEffectOnce' $ do
      T.listen (\{ new } -> afterCacheStateChange new) cacheState

    useLoader { errorHandler
              , loader: loadCorpusWithChild
              , path: { nodeId, session }
              , render: \corpusData@{ corpusId, corpusNode } ->
                  let
                    NodePoly { name, date, hyperdata } = corpusNode

                  in
                    H.div
                    { className: "texts-layout" }
                    [
                      Table.tableHeaderWithRenameLayout
                      { cacheState
                      , name
                      , date
                      , hyperdata
                      , nodeId: corpusId
                      , session
                      , key: "textsLayoutWithKey-" <> (show cacheState')
                      }
                    ,
                      tabs
                      { boxes
                      , cacheState
                      , corpusData
                      , corpusId
                      , eChartsInstance
                      , frontends
                      , session
                      , sidePanel: sidePanelTexts
                      , yearFilter
                      }
                    ]
              }
    where
      errorHandler = logRESTError here "[textsLayoutWithKey]"
      afterCacheStateChange cacheState = do
        launchAff_ $ clearCache unit
        -- TODO
        --sessionUpdate $ setCacheState session nodeId cacheState
        --_ <- setCacheState session nodeId cacheState

-----------------------------------------------------

data Mode = MoreLikeFav | MoreLikeTrash

derive instance Generic Mode _

instance Show Mode where
  show = genericShow

derive instance Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType MoreLikeFav    = CTabAuthors  -- TODO
modeTabType MoreLikeTrash  = CTabSources  -- TODO

type TabsProps =
  ( boxes           :: Boxes
  , cacheState      :: T.Box LT.CacheState
  , corpusData      :: CorpusData
  , corpusId        :: NodeID
  , eChartsInstance :: T.Box (Maybe EChartsInstance)
  , frontends       :: Frontends
  , session         :: Session
  , sidePanel       :: T.Box (Maybe (Record TT.SidePanel))
  , yearFilter      :: T.Box (Maybe Year)
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []
tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt
  where
    cpt { boxes
        , cacheState
        , corpusId
        , corpusData
        , eChartsInstance
        , frontends
        , session
        , sidePanel
        , yearFilter } _ = do

      let
        path = initialPath

        onInit = Just \i -> T.write_ (Just i) eChartsInstance

        onClick = Just \opts@{ name } -> do
          T.write_ (Just name) yearFilter
          T.read eChartsInstance >>= case _ of
            Nothing -> pure unit
            Just i  -> do
              -- @XXX due to lack of support for "echart.select" action,
              --      have to manually rely on a set/unset selection
              --      targeting the "echart.emphasis" action
              let
                opts' :: Record EChartActionData
                opts' =
                  { dataIndex   : opts.dataIndex
                  , name        : opts.name
                  , seriesId    : opts.seriesId
                  , seriesIndex : opts.seriesIndex
                  , seriesName  : opts.seriesName
                  , type        : "highlight"
                  }
              dispatchAction i { type: "downplay" }
              dispatchAction i opts'

      activeTab <- T.useBox 0

      pure $ Tab.tabs {
          activeTab
        , tabs: [
            "Documents"       /\ R.fragment [
                histo { boxes, path, session, onClick, onInit }
              , docView' path TabDocs
              ]
          , "Trash"           /\ docView' path TabTrash
          -- , "More like fav"   /\ docView' path TabMoreLikeFav
          -- , "More like trash" /\ docView' path TabMoreLikeTrash
          ]
        }

      where
        initialPath = { corpusId
                      , listId: corpusData.defaultListId
                      , limit: Nothing
                      , tabType: TabCorpus TabDocs }
        docView' path tabType = docView { boxes
                                        , cacheState
                                        , corpusData
                                        , corpusId
                                        , frontends
                                        , listId: path.listId
                                        -- , path
                                        , session
                                        , tabType
                                        , sidePanel
                                        , yearFilter
                                        } []

type DocViewProps a =
  ( boxes      :: Boxes
  , cacheState :: T.Box LT.CacheState
  , corpusData :: CorpusData
  , corpusId   :: NodeID
  , frontends  :: Frontends
  , listId     :: ListId
  -- , path    :: Record DT.Path
  , session    :: Session
  , tabType    :: TabSubType a
  , sidePanel  :: T.Box (Maybe (Record TT.SidePanel))
  , yearFilter :: T.Box (Maybe Year)
  )

docView :: forall a. R2.Component (DocViewProps a)
docView = R.createElement docViewCpt
docViewCpt :: forall a. R.Component (DocViewProps a)
docViewCpt = here.component "docView" cpt
  where
    cpt props _children = do
      pure $ DT.docViewLayout $ docViewLayoutRec props

-- docViewLayoutRec :: forall a. DocViewProps a -> Record DT.LayoutProps
docViewLayoutRec { boxes
                 , cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabDocs
                 , sidePanel
                 , yearFilter
                 } =
  { boxes
  , cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanel
  , tabType: TabCorpus TabDocs
  , totalRecords: 4737
  , yearFilter
  }
docViewLayoutRec { boxes
                 , cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabMoreLikeFav
                 , sidePanel
                 , yearFilter
                 } =
  { boxes
  , cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanel
  , tabType: TabCorpus TabMoreLikeFav
  , totalRecords: 4737
  , yearFilter
  }
docViewLayoutRec { boxes
                 , cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabMoreLikeTrash
                 , sidePanel
                 , yearFilter
                 } =
  { boxes
  , cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanel
  , tabType: TabCorpus TabMoreLikeTrash
  , totalRecords: 4737
  , yearFilter
  }
docViewLayoutRec { boxes
                 , cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabTrash
                 , sidePanel
                 , yearFilter
                 } =
  { boxes
  , cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanel
  , tabType: TabCorpus TabTrash
  , totalRecords: 4737
  , yearFilter
  }
-- DUMMY
docViewLayoutRec { boxes
                 , cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , sidePanel
                 , tabType
                 , yearFilter
                 } =
  { boxes
  , cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanel
  , tabType: TabCorpus TabTrash
  , totalRecords: 4737
  , yearFilter
  }


--------------------------------------------------------

textsSidePanel :: R2.Leaf ()
textsSidePanel = R2.leaf textsSidePanelCpt

textsSidePanelCpt :: R.Component ()
textsSidePanelCpt = here.component "sidePanel" cpt where
  cpt _ _ = do

    { sidePanelState
    , sidePanelTexts
    } <- AppStore.use

    session <- useSession

    sidePanelState' <- R2.useLive' sidePanelState
    sidePanelTexts' <- R2.useLive' sidePanelTexts

    -- R.useEffect' $ do
    --   let toggleSidePanel' _  = snd sidePanelState toggleSidePanelState
    --       triggerSidePanel' _ = snd sidePanelState $ const Opened
    --   R2.setTrigger toggleSidePanel  toggleSidePanel'
    --   R2.setTrigger triggerSidePanel triggerSidePanel'

    -- (mCorpusId /\ setMCorpusId) <- R.useState' Nothing
    -- (mListId /\ setMListId) <- R.useState' Nothing
    -- (mNodeId /\ setMNodeId) <- R.useState' Nothing

    -- R.useEffect3 mCorpusId mListId mNodeId $ do
    --   if mCorpusId == Just corpusId && mListId == Just listId && mNodeId == Just nodeId && mCurrentDocId == Just nodeId then do
    --     T.modify_ (\sp -> sp { mCurrentDocId = Nothing }) sidePanelTexts
    --   else do
    --     T.modify_ (\sp -> sp { mCorpusId = Just corpusId
    --                         , mCurrentDocId = Just nodeId
    --                         , mListId = Just listId
    --                         , mNodeId = Just nodeId }) sidePanelTexts
      -- let trigger :: Record TriggerAnnotatedDocIdChangeParams -> Effect Unit
      --     trigger { corpusId, listId, nodeId } = do
            -- log2 "[sidePanel trigger] trigger corpusId change" corpusId
            -- log2 "[sidePanel trigger] trigger listId change" listId
            -- log2 "[sidePanel trigger] trigger nodeId change" nodeId
            -- if mCorpusId == Just corpusId && mListId == Just listId && mNodeId == Just nodeId && mCurrentDocId == Just nodeId then do
              -- R.setRef currentDocIdRef Nothing
              -- T.modify_ (\sp -> sp { mCurrentDocId = Nothing }) sidePanelTexts
              -- R2.callTrigger toggleSidePanel unit
            -- else do
              -- setMCorpusId $ const $ Just corpusId
              -- setMListId $ const $ Just listId
              -- setMNodeId $ const $ Just nodeId
              -- R.setRef currentDocIdRef $ Just nodeId
              -- R2.callTrigger triggerSidePanel unit
              -- T.modify_ (\sp -> sp { mCorpusId = Just corpusId
              --                     , mCurrentDocId = Just nodeId
              --                     , mListId = Just listId
              --                     , mNodeId = Just nodeId }) sidePanelTexts
      -- log2 "[sidePanel] trigger" trigger
      -- R2.setTrigger triggerAnnotatedDocIdChange trigger
      -- pure unit

      -- pure $ do
      --   -- log "[sidePanel] clearing triggerAnnotatedDocIdChange"
      --   R2.clearTrigger triggerAnnotatedDocIdChange

    let closeSidePanel _ = do
          -- T.modify_ (\sp -> sp { mCurrentDocId = Nothing
          --                     , state = Closed }) sidePanelTexts
          T.write_ Closed sidePanelState
          T.write_ Nothing sidePanelTexts


    pure $

      H.div
      -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
      -- @link https://github.com/facebook/react/issues/12039
      { className: "texts-sidepanel"
      , style: { display: sidePanelState' == Opened ? "block" $ "none" }
      }
      [
        H.div
        { className: "texts-sidepanel__inner" }
        [
          H.div
          { className: "texts-sidepanel__header" }
          [
            B.iconButton
            { name: "times"
            , elevation: Level2
            , callback: closeSidePanel
            }
          ]
        ,
          H.div
          { className: "texts-sidepanel__body" }
          [
            case sidePanelTexts' of
              Nothing ->
                B.caveat
                {}
                [ H.text $ "You can select a document to see its content" ]

              Just { corpusId, listId, nodeId } ->
                D.node
                { listId
                , mCorpusId: Just corpusId
                , nodeId
                , key: show (sessionId session) <> "-" <> show nodeId
                }
          ]
        ]
      ]
