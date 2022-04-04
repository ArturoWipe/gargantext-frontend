module Gargantext.Components.Nodes.Corpus.Graph
  ( graphLayout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import FFI.Simple ((..), (.=))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.GraphExplorer.Layout (convert, layout)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, get)
import Gargantext.Types as Types
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Toestand as T


type Props =
  ( key       :: String
  , session   :: Session
  , boxes     :: Boxes
  , graphId   :: GET.GraphId
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Graph"

graphLayout :: R2.Leaf Props
graphLayout = R2.leaf graphLayoutCpt

graphLayoutCpt :: R.Component Props
graphLayoutCpt = here.component "explorerLayout" cpt where
  cpt props@{ boxes: { graphVersion }, graphId, session } _ = do
    graphVersion' <- T.useLive T.unequal graphVersion

    useLoader
      { errorHandler
      , loader: getNodes session graphVersion'
      , path: graphId
      , render: handler
      }

    where
      errorHandler = logRESTError here "[explorerLayout]"
      handler loaded@(GET.HyperdataGraph { graph: hyperdataGraph }) =
        content { graph
                , hyperdataGraph: loaded
                , mMetaData'
                , session
                , boxes: props.boxes
                , graphId
                }
        where
          Tuple mMetaData' graph = convert hyperdataGraph

--------------------------------------------------------

type ContentProps =
  ( mMetaData'      :: Maybe GET.MetaData
  , graph           :: SigmaxT.SGraph
  , hyperdataGraph  :: GET.HyperdataGraph
  , session         :: Session
  , boxes           :: Boxes
  , graphId         :: GET.GraphId
  )

content :: R2.Leaf ContentProps
content = R2.leaf contentCpt

contentCpt :: R.Component ContentProps
contentCpt = here.component "content" cpt where
  cpt props@{ boxes, mMetaData', graph } _ = do
  -- Hooks

    R.useEffectOnce' $
      -- Hydrate Boxes
      flip T.write_ boxes.sidePanelGraph $ Just
        { mGraph: Just graph
        , mMetaData: mMetaData'
        , multiSelectEnabled: false
        , removedNodeIds: Set.empty
        , selectedNodeIds: Set.empty
        , showControls: false
        , sideTab: GET.SideTabLegend
        }

    useFirstEffect' do
      -- @XXX: inopinent <div> (see Gargantext.Components.Router) (@TODO?)
      mEl <- querySelector document ".main-page__main-route .container"
      case mEl of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "display") "none"
      -- @XXX: reset "main-page__main-route" wrapper margin
      --       see Gargantext.Components.Router) (@TODO?)
      mEl' <- querySelector document ".main-page__main-route"
      case mEl' of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "padding") "initial"

  -- Render

    pure $

      layout
      props

--------------------------------------------------------------

getNodes :: Session -> T2.Reload -> GET.GraphId -> AffRESTError GET.HyperdataGraph
getNodes session graphVersion graphId =
  get session $ NodeAPI Types.Graph
                        (Just graphId)
                        ("?version=" <> (show graphVersion))
