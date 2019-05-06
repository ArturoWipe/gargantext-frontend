-- TODO copy of Gargantext.Pages.Corpus.Tabs.Specs
module Gargantext.Pages.Annuaire.User.Contacts.Tabs.Specs where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Config (TabType(..), TabSubType(..), PTabNgramType(..))
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Pages.Annuaire.User.Contacts.Types (Props)
import Thermite (Spec, focus, hideState, noState, cmapProps)

data Mode = Patents | Books | Communication

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> PTabNgramType
modeTabType Patents = PTabPatents
modeTabType Books = PTabBooks
modeTabType Communication = PTabCommunication

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  Tab.tabs identity identity $ fromFoldable
    [ Tuple "Documents"     $ docs
    , Tuple "Patents"       $ ngramsViewSpec {mode: Patents}
    , Tuple "Books"         $ ngramsViewSpec {mode: Books}
    , Tuple "Communication" $ ngramsViewSpec {mode: Communication}
    , Tuple "Trash"         $ docs -- TODO pass-in trash mode
    ]
  where
    chart = mempty
    -- TODO totalRecords
    docs = cmapProps (\{path: nodeId, loaded} ->
                       { nodeId, chart
                       , tabType: TabPairing TabDocs
                       , totalRecords: 4736
                       , listId: loaded.defaultListId}) $
           noState DT.docViewSpec

ngramsViewSpec :: {mode :: Mode} -> Spec Tab.State Props Tab.Action
ngramsViewSpec {mode} =
  cmapProps (\{loaded: {defaultListId}, path, dispatch} ->
              {loaded: {defaultListId}, path, dispatch, tabType})
            (noState NT.mainNgramsTableSpec)
    where
      tabType = TabPairing $ TabNgramType $ modeTabType mode
