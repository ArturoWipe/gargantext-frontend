module Gargantext.Components.Nodes.Corpus.Chart.Tree where

import Prelude (bind, pure, ($), (==))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (~>), (:=))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (TreeNode, Trees(..), mkTree)
import Gargantext.Components.Charts.Options.Font (mkTooltip, templateFormatter)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, Path, Props, ReloadPath)
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Tree"

newtype Metrics = Metrics {
    "data" :: Array TreeNode
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ Metrics { "data": d }
instance encodeMetrics :: EncodeJson Metrics where
  encodeJson (Metrics { "data": d }) =
       "data" := encodeJson d
    ~> jsonEmptyObject

type Loaded  = Array TreeNode

scatterOptions :: Record MetricsProps -> Array TreeNode -> Options
scatterOptions { onClick } nodes = Options
  { mainTitle : "Tree"
  , subTitle  : "Tree Sub Title"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position : "", show: false, min:0}
  , series    : [ mkTree TreeMap nodes]
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , onClick
-- TODO improve the formatter:
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-obama

  }

getMetricsHash :: Session -> ReloadPath -> Aff String
getMetricsHash session (_ /\ { corpusId, limit, listId, tabType }) = do
  get session $ ChartHash { chartType: ChartTree, listId: mListId, tabType } (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = Chart {chartType: ChartTree, limit, listId: mListId, tabType} (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

handleResponse :: HashedResponse Metrics -> Loaded
handleResponse (HashedResponse { value: Metrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path@{ corpusId, limit, listId, tabType }) = GUC.makeGetRequest session $ chartUrl path

tree :: Record Props -> R.Element
tree props = R.createElement treeCpt props []

treeCpt :: R.Component Props
treeCpt = here.component "tree" cpt
  where
    cpt {path, session, onClick} _ = do
      reload <- T.useBox T2.newReload

      pure $ metricsWithCacheLoadView {
          getMetricsHash
        , handleResponse
        , loaded
        , mkRequest: mkRequest session
        , path
        , reload
        , session
        , onClick
        }

loaded :: Record MetricsProps -> Loaded -> R.Element
loaded p@{ path, reload, session } loaded' =
  H.div {} [
  {-  U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartTree, path, reload, session }
  , -} chart (scatterOptions p loaded')
  ]
