module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Components.Nodes.Corpus.Chart.Types (Reload, Path, Props, MetricsProps, ReloadPath)
import Gargantext.Hooks.Loader (MD5, HashedResponse, useLoader, useLoaderWithCache, useLoaderWithCacheAPI)
import Gargantext.Sessions (Session)
import Gargantext.Utils.CacheAPI as GUC

type MetricsLoadViewProps a = (
    getMetrics :: Session -> ReloadPath -> Aff a
  , loaded :: Record MetricsProps -> a -> R.Element
  | MetricsProps
  )

cacheName :: String
cacheName = "metrics"

metricsLoadView :: forall a. Record (MetricsLoadViewProps a) -> R.Element
metricsLoadView p = R.createElement metricsLoadViewCpt p []

metricsLoadViewCpt :: forall a. R.Component (MetricsLoadViewProps a)
metricsLoadViewCpt = R.hooksComponent "G.C.N.C.C.metricsLoadView" cpt
  where
    cpt { getMetrics, loaded, path, reload, session } _ = do
      useLoader (fst reload /\ path) (getMetrics session) $ \l ->
        loaded { path, reload, session } l

type MetricsWithCacheLoadViewProps res ret = (
    getMetricsMD5 :: Session -> ReloadPath -> Aff MD5
  , handleResponse :: HashedResponse res -> ret
  , loaded :: Record MetricsProps -> ret -> R.Element
  , mkRequest :: ReloadPath -> GUC.Request
  | MetricsProps
  )

metricsWithCacheLoadView :: forall res ret. DecodeJson res =>
                            Record (MetricsWithCacheLoadViewProps res ret) -> R.Element
metricsWithCacheLoadView p = R.createElement metricsWithCacheLoadViewCpt p []

metricsWithCacheLoadViewCpt :: forall res ret. DecodeJson res =>
                               R.Component (MetricsWithCacheLoadViewProps res ret)
metricsWithCacheLoadViewCpt = R.hooksComponent "G.C.N.C.C.metricsWithCacheLoadView" cpt
  where
    cpt { getMetricsMD5, handleResponse, loaded, mkRequest, path, reload, session } _ = do
    --   useLoaderWithCache (fst reload /\ path) (metricsKeyFunc keyFunc) (getMetricsMD5 session) (getMetrics session) $ \l ->
    --     loaded session path reload l
    -- metricsKeyFunc keyFunc st@(_ /\ { corpusId, listId, tabType }) =
    --  "metrics-" <> (show tabType) <> "-" <> (show corpusId) <> "-" <> (show listId) <> "--" <> (keyFunc st)

      useLoaderWithCacheAPI { cacheEndpoint: (getMetricsMD5 session)
                            , handleResponse
                            , mkRequest
                            , path: (fst reload /\ path)
                            , renderer: loaded { path, reload, session } }
