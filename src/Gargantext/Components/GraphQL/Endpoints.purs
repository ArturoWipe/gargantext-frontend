module Gargantext.Components.GraphQL.Endpoints where

import Gargantext.Prelude

import Gargantext.Components.GraphQL.Node (Node, nodeParentQuery)
import Gargantext.Components.GraphQL.Tree (TreeFirstLevel, treeFirstLevelQuery)
import Gargantext.Components.GraphQL.User (UserInfo, userInfoQuery)

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL (queryGql)
import Gargantext.Components.GraphQL.IMT as GQLIMT
import Gargantext.Config.REST (RESTError(..), AffRESTError)
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeType)
import Gargantext.Utils.Reactix as R2
import Gargnatext.Components.GraphQL.Contact (AnnuaireContact, annuaireContactQuery)
import GraphQL.Client.Variables (withVars)

here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL.Endpoints"

getIMTSchools :: Session -> AffRESTError (Array GQLIMT.School)
getIMTSchools session = do
  { imt_schools } <- queryGql session "get imt schools" $
                         GQLIMT.schoolsQuery
  liftEffect $ here.log2 "[getIMTSchools] imt_schools" imt_schools
  pure $ Right imt_schools

getNodeParent :: Session -> Int -> NodeType -> Aff (Array Node)
getNodeParent session nodeId parentType = do
  { node_parent } <- queryGql session "get node parent" $
                     nodeParentQuery `withVars` { id: nodeId
                                                , parent_type: show parentType }  -- TODO: remove "show"
  liftEffect $ here.log2 "[getNodeParent] node_parent" node_parent
  pure node_parent

getUserInfo :: Session -> Int -> AffRESTError UserInfo
getUserInfo session id = do
  { user_infos } <- queryGql session "get user infos" $ userInfoQuery `withVars` { id }
  liftEffect $ here.log2 "[getUserInfo] user infos" user_infos
  pure $ case A.head user_infos of
    Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
    -- NOTE Contact is at G.C.N.A.U.C.Types
    Just ui -> Right ui

getAnnuaireContact :: Session -> Int -> AffRESTError AnnuaireContact
getAnnuaireContact session id = do
  { annuaire_contacts } <- queryGql session "get annuaire contact" $
    annuaireContactQuery `withVars` { id }
  liftEffect $ here.log2 "[getAnnuaireContact] data" annuaire_contacts
  pure $ case A.head annuaire_contacts of
    Nothing -> Left (CustomError $ "contact id=" <> show id <> " not found")
    Just r  -> Right r

getTreeFirstLevel :: Session -> Int -> AffRESTError TreeFirstLevel
getTreeFirstLevel session id = do
  { tree } <- queryGql session "get tree first level" $ treeFirstLevelQuery `withVars` { id }
  liftEffect $ here.log2 "[getTreeFirstLevel] tree first level" tree
  pure $ Right tree -- TODO: error handling
