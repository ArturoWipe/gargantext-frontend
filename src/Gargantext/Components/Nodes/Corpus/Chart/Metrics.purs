module Gargantext.Components.Nodes.Corpus.Chart.Metrics where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (~>), (:=))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude (class Eq, bind, negate, pure, ($), (<$>), (<>))

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, yAxis')
import Gargantext.Components.Charts.Options.Type (xAxis)
import Gargantext.Components.Charts.Options.Series (Series, seriesScatterD2)
import Gargantext.Components.Charts.Options.Color (green, grey, red)
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types
  (MetricsProps, Path, Props, ReloadPath)
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (TermList(..))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Metrics"

newtype Metric = Metric
  { label :: String
  , x     :: Number
  , y     :: Number
  , cat   :: TermList
  }
derive instance genericMetric :: Generic Metric _
instance eqMetric :: Eq Metric where
  eq = genericEq
instance decodeMetric :: DecodeJson Metric where
  decodeJson json = do
    obj   <- decodeJson json
    label <- obj .: "label"
    x     <- obj .: "x"
    y     <- obj .: "y"
    cat   <- obj .: "cat"
    pure $ Metric { label, x, y, cat }
instance encodeMetric :: EncodeJson Metric where
  encodeJson (Metric { label, x, y, cat }) =
       "label"  := encodeJson label
    ~> "x"      := encodeJson x
    ~> "y"      := encodeJson y
    ~> "cat"    := encodeJson cat
    ~> jsonEmptyObject

newtype Metrics = Metrics {
     "data" :: Array Metric
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ Metrics { "data": d }

type Loaded  = Array Metric

scatterOptions :: Record MetricsProps -> Array Metric -> Options
scatterOptions { onClick } metrics' = Options
  { mainTitle : "Ngrams Selection Metrics"
  , subTitle  : "Local metrics (Inc/Exc, Spe/Gen), Global metrics (TFICF maillage)"
  , xAxis     : xAxis { min: -1 }
  , yAxis     : yAxis' { position : "", show: true, min : -2}
  , series    : map2series $ metric2map metrics'
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , onClick
  }
  where
    metric2map :: Array Metric -> Map TermList (Array Metric)
    metric2map ds = Map.fromFoldableWith (<>) $ (\(Metric m) -> Tuple m.cat [Metric m]) <$> ds

    --{-
    map2series :: Map TermList (Array Metric) -> Array Series
    map2series ms = toSeries <$> Map.toUnfoldable ms
      where
        -- TODO colors are not respected yet
        toSeries (Tuple k ms') =
            seriesScatterD2 {symbolSize: 5.0} (toSerie color <$> ms')
          where
            color =
              case k of
                StopTerm -> red
                MapTerm -> green
                CandidateTerm -> grey
            toSerie color' (Metric {label,x,y}) =
              dataSerie { name: label, itemStyle: itemStyle {color: color'}
                     -- , label: {show: true}
                        , value: [x,y]
                        }
    --}

getMetricsHash :: Session -> ReloadPath -> Aff String
getMetricsHash session (_ /\ { corpusId, listId, tabType }) =
  get session $ CorpusMetricsHash { listId, tabType } (Just corpusId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = CorpusMetrics { limit, listId, tabType } (Just corpusId)

handleResponse :: HashedResponse Metrics -> Loaded
handleResponse (HashedResponse { value: Metrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path@{ corpusId, limit, listId, tabType }) = GUC.makeGetRequest session $ chartUrl path

metrics :: Record Props -> R.Element
metrics props = R.createElement metricsCpt props []

metricsCpt :: R.Component Props
metricsCpt = here.component "etrics" cpt
  where
    cpt {path, session, onClick } _ = do
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
  , U.chartUpdateButton { chartType: Scatter, path, reload, session }
  , -} chart $ scatterOptions p loaded'
  ]
