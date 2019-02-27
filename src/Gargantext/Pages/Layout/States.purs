module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Data.Lens                                       (Lens', lens)
import Data.Maybe                                      (Maybe(Just))
import Effect (Effect)
import Gargantext.Components.Login                  as LN

import Gargantext.Pages.Corpus.Document       as D
import Gargantext.Pages.Corpus.Graph     as GE
import Gargantext.Pages.Layout.Specs.AddCorpus      as AC
import Gargantext.Pages.Layout.Specs.Search         as S
import Gargantext.Pages.Layout.Specs.SearchBar      as SB
import Gargantext.Router                               (Routes(..))

type AppState =
  { currentRoute       :: Maybe Routes
  , loginState         :: LN.State
  , addCorpusState     :: AC.State
  , searchState        :: S.State
  , documentState      :: D.State
  , searchBarState     :: SB.State
  , showLogin          :: Boolean
  , showCorpus         :: Boolean
  , graphExplorerState :: GE.State
  , showTree           :: Boolean
  }

initAppState :: Effect AppState
initAppState = do
  loginState <- LN.initialState
  pure
    { currentRoute   : Just Home
    , loginState
    , addCorpusState : AC.initialState
    , searchState    : S.initialState
    , documentState  : D.initialState {}
    , searchBarState : SB.initialState
    , showLogin      : false
    , showCorpus     : false
    , graphExplorerState : GE.initialState
    , showTree : false
    }


---------------------------------------------------------

_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_addCorpusState :: Lens' AppState AC.State
_addCorpusState = lens (\s -> s.addCorpusState) (\s ss -> s{addCorpusState = ss})

_searchState :: Lens' AppState S.State
_searchState = lens (\s -> s.searchState) (\s ss -> s{searchState = ss})

_searchBarState :: Lens' AppState SB.State
_searchBarState = lens (\s -> s.searchBarState) (\s ss -> s{searchBarState = ss})

_documentViewState :: Lens' AppState D.State
_documentViewState = lens (\s -> s.documentState) (\s ss -> s{documentState = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorerState) (\s ss -> s{graphExplorerState = ss})

