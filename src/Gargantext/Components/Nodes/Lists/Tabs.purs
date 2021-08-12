module Gargantext.Components.Nodes.Lists.Tabs where

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Components.Nodes.Corpus.Chart.Utils (mNgramsTypeFromTabType)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
import Gargantext.Components.Nodes.Lists.Types
import Gargantext.Components.Tab as Tab
import Gargantext.Prelude (bind, pure, unit, ($), (<>))
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), FrontendError, Mode(..), TabSubType(..), TabType(..), modeTabType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists.Tabs"

type Props = (
    activeTab    :: T.Box Int
  , cacheState   :: T.Box CacheState
  , corpusData   :: CorpusData
  , corpusId     :: Int
  , errors       :: T.Box (Array FrontendError)
  , reloadForest :: T2.ReloadS
  , reloadRoot   :: T2.ReloadS
  , session      :: Session
  , tasks        :: T.Box GAT.Storage
  )

type PropsWithKey = ( key :: String | Props )

tabs :: Record PropsWithKey -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component PropsWithKey
tabsCpt = here.component "tabs" cpt where
  cpt props@{ activeTab } _ = do
    pure $ Tab.tabs { activeTab
                    , tabs: tabs' } where
      tabs' = [ "Terms"      /\ view Terms []
              , "Authors"    /\ view Authors []
              , "Institutes" /\ view Institutes []
              , "Sources"    /\ view Sources []
              ]
      common = RX.pick props :: Record Props
      view mode = ngramsView $ Record.merge common { mode }

type NgramsViewProps = ( mode :: Mode | Props )

ngramsView :: R2.Component NgramsViewProps
ngramsView = R.createElement ngramsViewCpt
ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = here.component "ngramsView" cpt where
  cpt props@{ cacheState
            , corpusData: { defaultListId }
            , corpusId
            , errors
            , reloadForest
            , reloadRoot
            , mode
            , session
            , tasks } _ = do
      chartsReload <- T.useBox T2.newReload

      path <- T.useBox $ NTC.initialPageParams props.session initialPath.corpusId [initialPath.listId] initialPath.tabType
      { listIds, nodeId, params, tabType } <- T.useLive T.unequal path
      let path' = {
          corpusId: nodeId
        , limit: params.limit
        , listId: fromMaybe defaultListId $ A.head listIds
        , tabType: tabType
        }
      let chartParams = {
          corpusId: path'.corpusId
        , limit: Just path'.limit
        , listId: path'.listId
        , tabType: path'.tabType
        }

      pure $ R.fragment
        ( charts chartParams tabNgramType
        <> [ NT.mainNgramsTable { afterSync: afterSync chartsReload
                                , cacheState
                                , defaultListId
                                , path
                                , reloadForest
                                , reloadRoot
                                , session
                                , tabNgramType
                                , tabType
                                , tasks
                                , withAutoUpdate: false
                                } []
           ]
        )
      where
        afterSync chartsReload _ = do
          case mNgramsType of
            Just ngramsType -> do
              -- NOTE: No need to recompute chart, after ngrams are sync this
              -- should be recomputed already
              -- We just refresh it
              -- _ <- recomputeChart session chartType ngramsType corpusId listId
              liftEffect $ T2.reload chartsReload
            Nothing         -> pure unit

        tabNgramType = modeTabType mode
        tabType      = TabCorpus (TabNgramType tabNgramType)
        mNgramsType  = mNgramsTypeFromTabType tabType
        listId       = defaultListId
        initialPath  = { corpusId
                       -- , limit: Just 1000
                       , listId
                       , tabType
                       }

        charts params CTabTerms = [
          H.div {className: "row"}
                [ H.div {className: "col-12 d-flex justify-content-center"}
                  [ H.img { src: "images/Gargantextuel-212x300.jpg"
                          , id: "funnyimg"
                        }
                  ]
                ]

          {-
              R2.select { className: "form-control"
                        , defaultValue: show chartType
                        , on: { change: \e -> setChartType
                                             $ const
                                             $ fromMaybe Histo
                                             $ chartTypeFromString
                                             $ R.unsafeEventValue e
                              }
                        } [
                H.option { value: show Histo     } [ H.text $ show Histo     ]
              , H.option { value: show Scatter   } [ H.text $ show Scatter   ]
              , H.option { value: show ChartBar  } [ H.text $ show ChartBar  ]
              , H.option { value: show ChartPie  } [ H.text $ show ChartPie  ]
              , H.option { value: show ChartTree } [ H.text $ show ChartTree ]
              ]
            ]
          ]
        , getChartFunction chartType $ { path: params, session }
        -}
        ]
        charts params _        = [ chart params mode ]

        chart path Authors    = pie     { errors, path, session, onClick: Nothing, onInit: Nothing }
        chart path Institutes = tree    { errors, path, session, onClick: Nothing, onInit: Nothing }
        chart path Sources    = bar     { errors, path, session, onClick: Nothing, onInit: Nothing }
        chart path Terms      = metrics { errors, path, session, onClick: Nothing, onInit: Nothing }
