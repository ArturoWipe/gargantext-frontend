-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs where

import Prelude hiding (div)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (ContactData')
import Gargantext.Components.Nodes.Lists.Types as LTypes
import Gargantext.Components.Nodes.Texts.Types as TTypes
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), PTabNgramType(..), TabType(..), TabSubType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs"


data Mode = Patents | Books | Communication

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> PTabNgramType
modeTabType Patents = PTabPatents
modeTabType Books = PTabBooks
modeTabType Communication = PTabCommunication

-- TODO fix this type
modeTabType' :: Mode -> CTabNgramType
modeTabType' Patents = CTabAuthors
modeTabType' Books = CTabAuthors
modeTabType' Communication = CTabAuthors

type TabsProps = (
    cacheState        :: R.State LTypes.CacheState
  , contactData       :: ContactData'
  , frontends         :: Frontends
  , nodeId            :: Int
  , reloadForest      :: T.Cursor T2.Reload
  , reloadRoot        :: T.Cursor T2.Reload
  , session           :: Session
  , sidePanelTriggers :: Record LTypes.SidePanelTriggers
  , tasks             :: T.Cursor (Maybe GAT.Reductor)
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt
  where
    cpt { reloadRoot
        , tasks
        , cacheState
        , contactData: {defaultListId}
        , frontends
        , nodeId
        , session
        , sidePanelTriggers
        , reloadForest } _ = do
      active <- R.useState' 0
      textsSidePanelTriggers <- TTypes.emptySidePanelTriggers
      pure $ Tab.tabs { selected: fst active, tabs: tabs' textsSidePanelTriggers }
      where
        tabs' trg =
          [ "Documents"     /\ docs
          , "Patents"       /\ ngramsView patentsView []
          , "Books"         /\ ngramsView booksView []
          , "Communication" /\ ngramsView commView []
          , "Trash"         /\ docs -- TODO pass-in trash mode
          ]
          where
            patentsView = { reloadRoot
                          , tasks
                          , cacheState
                          , defaultListId
                          , mode: Patents
                          , nodeId
                          , session
                          , sidePanelTriggers
                          , reloadForest }
            booksView   = { reloadRoot
                          , tasks
                          , cacheState
                          , defaultListId
                          , mode: Books
                          , nodeId
                          , session
                          , sidePanelTriggers
                          , reloadForest }
            commView    = { reloadRoot, tasks
                          , cacheState
                          , defaultListId
                          , mode: Communication
                          , nodeId
                          , session
                          , sidePanelTriggers
                          , reloadForest }
            chart       = mempty
            totalRecords = 4736 -- TODO
            docs = DT.docViewLayout
              { cacheState
              , chart
              , frontends
              , listId: defaultListId
              , mCorpusId: Nothing
              , nodeId
              , session
              , showSearch: true
              , sidePanelTriggers: trg
              , tabType: TabPairing TabDocs
              , totalRecords
              }


type NgramsViewTabsProps = (
    cacheState        :: R.State LTypes.CacheState
  , defaultListId     :: Int
  , mode              :: Mode
  , nodeId            :: Int
  , reloadForest      :: T.Cursor T2.Reload
  , reloadRoot        :: T.Cursor T2.Reload
  , session           :: Session
  , sidePanelTriggers :: Record LTypes.SidePanelTriggers
  , tasks             :: T.Cursor (Maybe GAT.Reductor)
  )

ngramsView :: R2.Component NgramsViewTabsProps
ngramsView = R.createElement ngramsViewCpt

ngramsViewCpt :: R.Component NgramsViewTabsProps
ngramsViewCpt = here.component "ngramsView" cpt
  where
    cpt { reloadRoot
        , tasks
        , cacheState
        , defaultListId
        , mode
        , nodeId
        , session
        , sidePanelTriggers
        , reloadForest } _ = do
      path <- R.useState' $ NTC.initialPageParams session nodeId [defaultListId] (TabDocument TabDocs)

      pure $ NT.mainNgramsTable {
          reloadRoot
        , afterSync: \_ -> pure unit
        , tasks
        , cacheState
        , defaultListId
        , nodeId
        , path
        , tabType
        , session
        , sidePanelTriggers
        , tabNgramType
        , reloadForest
        , withAutoUpdate: false
        } []
      where
        tabNgramType = modeTabType' mode
        tabType      = TabPairing $ TabNgramType $ modeTabType mode
