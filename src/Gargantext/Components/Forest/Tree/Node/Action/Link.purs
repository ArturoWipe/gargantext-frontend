module Gargantext.Components.Forest.Tree.Node.Action.Link
  where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (SubTreeParamsProps, subTreeView, SubTreeOut(..))
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, put_)
import Gargantext.Types  as GT
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Reactix as R
import Reactix.DOM.HTML as H

linkNodeReq :: Session -> GT.ID -> GT.ID -> Aff (Array GT.ID)
linkNodeReq session fromId toId =
  put_ session $ NodeAPI GT.Node (Just fromId) ("link/" <> show toId)

linkNode :: Record SubTreeParamsProps -> R.Hooks R.Element
linkNode p@{subTreeOut, dispatch} = pure $ panel [subTreeView p] button
  where
    ( subTreeOutParams /\ _ ) = subTreeOut
    button = case subTreeOutParams of
      Nothing   -> H.div {} []
      Just sbto -> submitButton (LinkNode inId outId) dispatch
        where
          (SubTreeOut { in:inId, out:outId}) = sbto
