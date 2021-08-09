module Gargantext.Components.NgramsTable.Loader where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, isJust)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Components.NgramsTable.Core (Version, Versioned(..))
import Gargantext.Config.REST (RESTError)
import Gargantext.Utils.CacheAPI as GUC
import Reactix as R
import Simple.JSON as JSON
import Toestand as T


cacheName :: String
cacheName = "ngrams-cache-api-loader"


clearCache :: Unit -> Aff Unit
clearCache _ = GUC.delete $ GUC.CacheName cacheName


type LoaderWithCacheAPIProps path res ret = (
    cacheEndpoint :: path -> Aff (Either RESTError Version)
  , handleResponse :: Versioned res -> ret
  , mkRequest :: path -> GUC.Request
  , path :: path
  , renderer :: ret -> R.Element
  )


useLoaderWithCacheAPI :: forall path res ret. Eq path => JSON.ReadForeign res => Eq ret =>
                         Record (LoaderWithCacheAPIProps path res ret)
                      -> R.Hooks R.Element
useLoaderWithCacheAPI { cacheEndpoint, handleResponse, mkRequest, path, renderer } = do
  state <- T.useBox Nothing
  state' <- T.useLive T.unequal state

  useCachedAPILoaderEffect { cacheEndpoint
                           , handleResponse
                           , mkRequest
                           , path
                           , state }
  pure $ maybe (loadingSpinner {}) renderer state'

type LoaderWithCacheAPIEffectProps path res ret = (
    cacheEndpoint  :: path -> Aff (Either RESTError Version)
  , handleResponse :: Versioned res -> ret
  , mkRequest      :: path -> GUC.Request
  , path           :: path
  , state          :: T.Box (Maybe ret)
  )

useCachedAPILoaderEffect :: forall path res ret. Eq path => JSON.ReadForeign res => Eq ret =>
                            Record (LoaderWithCacheAPIEffectProps path res ret)
                         -> R.Hooks Unit
useCachedAPILoaderEffect { cacheEndpoint
                         , handleResponse
                         , mkRequest
                         , path
                         , state: state } = do
  oPath <- R.useRef path
  state' <- T.useLive T.unequal state

  R.useEffect' $ do
    if (R.readRef oPath == path) && (isJust state') then
      pure unit
    else do
      R.setRef oPath path

      let req = mkRequest path
      -- log2 "[useCachedLoader] mState" mState
      launchAff_ $ do
        cache <- GUC.openCache $ GUC.CacheName cacheName
        -- TODO Parallelize?
        vr@(Versioned { version, "data": d }) <- GUC.cachedJson cache req
        eCacheReal <- cacheEndpoint path
        case eCacheReal of
          Left err -> throwError $ error $ "[useCachedAPILoaderEffect] RESTError"
          Right cacheReal -> do
            val <- if version == cacheReal then
              pure vr
            else do
              -- liftEffect $ do
              --   log "[useCachedAPILoaderEffect] versions dont match"
              --   log2 "[useCachedAPILoaderEffect] cached version" version
              --   log2 "[useCachedAPILoaderEffect] real version" cacheReal
              _ <- GUC.deleteReq cache req
              vr'@(Versioned { version: version', data: _ }) <- GUC.cachedJson cache req
              if version' == cacheReal then
               pure vr'
              else
                throwError $ error $ "[useCachedAPILoaderEffect] Fetched clean cache but hashes don't match: " <> show version <> " != " <> show cacheReal
            liftEffect $ do
              T.write_ (Just $ handleResponse val) state
