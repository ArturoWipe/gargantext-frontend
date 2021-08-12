module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, ReloadPath)
import Gargantext.Config.REST (RESTError)
import Gargantext.Hooks.Loader (HashedResponse, useLoader, useLoaderWithCacheAPI)
import Gargantext.Sessions (Session)
import Gargantext.Types (FrontendError(..))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Common"

type MetricsLoadViewProps a = (
    getMetrics :: Session -> ReloadPath -> Aff (Either RESTError a)
  , loaded :: Record MetricsProps -> a -> R.Element
  | MetricsProps
  )

cacheName :: String
cacheName = "metrics"

metricsLoadView :: forall a. Eq a => Record (MetricsLoadViewProps a) -> R.Element
metricsLoadView p = R.createElement metricsLoadViewCpt p []
metricsLoadViewCpt :: forall a. Eq a => R.Component (MetricsLoadViewProps a)
metricsLoadViewCpt = here.component "metricsLoadView" cpt
  where
    cpt { errors, getMetrics, loaded, onClick, onInit, path, reload, session } _ = do
      reload' <- T.useLive T.unequal reload

      useLoader { errorHandler
                , loader: getMetrics session
                , path: reload' /\ path
                , render: \l -> loaded { errors, path, reload, session, onClick, onInit } l }
      where
        errorHandler error = do
          T.modify_ (A.cons $ FRESTError { error }) errors
          here.log2 "RESTError" error

type MetricsWithCacheLoadViewProps res ret =
  ( getMetricsHash :: Session -> ReloadPath -> Aff (Either RESTError Hash)
  , handleResponse :: HashedResponse res -> ret
  , loaded         :: Record MetricsProps -> ret -> R.Element
  , mkRequest      :: ReloadPath -> GUC.Request
  | MetricsProps
  )

metricsWithCacheLoadView :: forall res ret.
                            Eq ret => JSON.ReadForeign res =>
                            Record (MetricsWithCacheLoadViewProps res ret) -> R.Element
metricsWithCacheLoadView p = R.createElement metricsWithCacheLoadViewCpt p []
metricsWithCacheLoadViewCpt :: forall res ret.
                               Eq ret => JSON.ReadForeign res =>
                               R.Component (MetricsWithCacheLoadViewProps res ret)
metricsWithCacheLoadViewCpt = here.component "metricsWithCacheLoadView" cpt
  where
    cpt { errors
        , getMetricsHash
        , handleResponse
        , loaded
        , mkRequest
        , path
        , reload
        , session
        , onClick
        , onInit } _ = do
      reload' <- T.useLive T.unequal reload

      useLoaderWithCacheAPI { cacheEndpoint: (getMetricsHash session)
                            , errors
                            , handleResponse
                            , mkRequest
                            , path: (reload' /\ path)
                            , renderer: loaded { errors, path, reload, session, onClick, onInit } }
