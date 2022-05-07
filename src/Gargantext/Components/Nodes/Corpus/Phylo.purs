module Gargantext.Components.Nodes.Corpus.Phylo
  ( node
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.API (get)
import Gargantext.Components.PhyloExplorer.Layout (layout)
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (PhyloDataSet)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record

type MainProps =
  ( nodeId      :: NodeID
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Phylo"

node :: R2.Leaf MainProps
node = R2.leaf nodeCpt

nodeCpt :: R.Component MainProps
nodeCpt = here.component "node" cpt where
  cpt { nodeId } _ = do
    -- | States
    -- |
    session <- useSession

    state' /\ state <- R2.useBox' Nothing

    -- | Computed
    -- |
    let

      errorHandler = logRESTError here "[phylo]"

      handler (phyloDataSet :: PhyloDataSet) =
        hydrateStore
        { phyloId: nodeId
        , phyloDataSet
        }


    -- | Hooks
    -- |

    useLoaderEffect
      { errorHandler
      , loader: get session
      , path: nodeId
      , state
      }

    -- @XXX: reset "main-page__main-route" wrapper margin
    --       see Gargantext.Components.Router) (@TODO?)
    R.useLayoutEffect1 [] do
      let mEl = querySelector document ".main-page__main-route"
      -- Mount
      mEl >>= maybe R.nothing (flip R2.addClass ["p-0"])
      -- Unmount
      pure $
        mEl >>= maybe R.nothing (flip R2.removeClass ["p-0"])


    -- | Render
    -- |
    pure $

      B.cloak
      { isDisplayed: isJust state'
      , idlingPhaseDuration: Just 150
      , cloakSlot:
          -- mimicking `PhyloExplorer.layout` preloading template
          H.div
          { className: "phylo" }
          [
            H.div
            { className: "phylo__spinner-wrapper" }
            [
              B.spinner
              { className: "phylo__spinner" }
            ]
          ]
      , defaultSlot:
          R2.fromMaybe_ state' handler
      }

--------------------------------------------------------

type HydrateStoreProps =
  ( phyloDataSet :: PhyloDataSet
  , phyloId      :: NodeID
  )

hydrateStore :: R2.Leaf HydrateStoreProps
hydrateStore = R2.leaf hydrateStoreCpt

hydrateStoreCpt :: R.Component HydrateStoreProps
hydrateStoreCpt = here.component "layout" cpt where
  cpt { phyloDataSet
      , phyloId
      } _ = do
    -- | Computed
    -- |
    let
      state :: Record PhyloStore.State
      state =
        -- Data
        { phyloDataSet
        , phyloId
        -- (default options)
        } `Record.merge` PhyloStore.options

    -- | Render
    -- |
    pure $

      PhyloStore.provide
      state
      [
        layout
        {}
      ]
