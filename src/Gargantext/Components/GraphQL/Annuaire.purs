module Gargantext.Components.GraphQL.Annuaire where

import Gargantext.Prelude

import Data.Array as A
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import GraphQL.Client.Args (NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))


type AnnuaireContact
  = { ac_id        :: Int
    , ac_firstName :: Maybe String
    , ac_lastName  :: Maybe String }

annuaireContactQuery =
  { annuaire_contacts: { contact_id: Var :: _ "id" Int } =>>
    { ac_id: unit
    , ac_firstName: unit
    , ac_lastName: unit
    }
  }

_ac_firstName :: Lens' AnnuaireContact String
_ac_firstName = lens getter setter
  where
    getter ({ ac_firstName: val }) = fromMaybe "" val
    setter ui val = ui { ac_firstName = Just val }
_ac_lastName :: Lens' AnnuaireContact String
_ac_lastName = lens getter setter
  where
    getter ({ ac_lastName: val }) = fromMaybe "" val
    setter ui val = ui { ac_lastName = Just val }

