module Gargantext.Components.PhyloExplorer.API
  ( get, update) where

import Gargantext.Prelude

import Gargantext.Components.PhyloExplorer.JSON (PhyloJSONSet)
import Gargantext.Components.PhyloExplorer.Types (PhyloDataSet, parsePhyloJSONSet)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions as S
import Gargantext.Types (NodeID)


get :: S.Session -> NodeID -> AffRESTError (PhyloDataSet)
get session nodeId = request >>= (_ <#> parsePhyloJSONSet) >>> pure
  where
    request :: AffRESTError (PhyloJSONSet)
    request = S.get session $ PhyloAPI nodeId

-- @WIP: change `Unit` to actual Record of options
update :: S.Session -> NodeID -> Unit -> AffRESTError (PhyloDataSet)
update session nodeId _ = request >>= (_ <#> parsePhyloJSONSet) >>> pure
  where
    request :: AffRESTError (PhyloJSONSet)
    request = S.post session (PhyloAPI nodeId) options

    options =
      { phyloProximity: 0.5
      , phyloSynchrony: 0.5
      , phyloQuality: 0.5
      , timeUnit: 3
      , clique: 5
      , exportFilter: [ 3 ]
      }
