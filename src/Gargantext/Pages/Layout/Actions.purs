module Gargantext.Pages.Layout.Actions where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)

import Gargantext.Pages.Corpus.Doc.Annotation   as D
import Gargantext.Pages.Corpus.Doc.Body         as CA
import Gargantext.Pages.Corpus.Doc.Document     as DV
import Gargantext.Components.Login              as LN
import Gargantext.Components.Modals.Modal          (modalShow)
import Gargantext.Components.Tree               as Tree
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus                  as AC
import Gargantext.Router                           (Routes(..))
import Gargantext.Pages.Corpus.User.Users       as U
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Home                    as L
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Search                  as S
import Gargantext.Pages.Corpus.Doc.Facets       as TV

import Gargantext.Pages.Layout.States (AppState)
import Gargantext.Pages.Corpus.Doc.Document as DV

import Network.HTTP.Affjax (AJAX)
import Thermite (PerformAction, modifyState)



data Action
  = Initialize
  | LandingA   L.Action
  | LoginA     LN.Action
  | SetRoute   Routes
  | AddCorpusA AC.Action
  | DocViewA   DV.Action
  | SearchA    S.Action
  | UserPageA  U.Action
  | DocAnnotationViewA  D.Action
  | TreeViewA  Tree.Action
  | TabViewA   TV.Action
  | GraphExplorerA GE.Action
  | DashboardA Dsh.Action
  | Search     String
  | Go
  | CorpusAnalysisA CA.Action
  | ShowLogin
  | ShowAddcorpus
  | NgramsA    NG.Action


performAction :: forall eff props. PerformAction ( dom :: DOM
                                                 , ajax :: AJAX
                                                 , console :: CONSOLE
                                                 | eff
                                                 ) AppState props Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}
performAction (Search s)  _ _ = void do
  modifyState $ _ {search = s}

performAction (ShowLogin)  _ _ = void do
  liftEff $ modalShow "loginModal"
  modifyState $ _ {showLogin = true}

performAction (ShowAddcorpus)  _ _ = void do
  liftEff $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}

performAction Go  _ _ = void do
  liftEff $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}
 -- _ <- lift $ setHash "/addCorpus"
  --modifyState id

performAction Initialize  _ state = void do
  _ <- liftEff $ log "loading Initial nodes"
  case state.initialized of
    false -> do

      lnodes <- lift $ Tree.loadDefaultNode

      case lnodes of
        Left err -> do
          modifyState id
        Right d -> do
          page <- lift $ DV.loadPage
          case page of
            Left err -> do
              modifyState id
            Right docs -> do
              modifyState $ _ { initialized = true
                              , ntreeView = if length d > 0
                                            then Tree.exampleTree
                                           --then fnTransform $ unsafePartial $ fromJust $ head d
                                           else Tree.initialState

                              , docViewState = docs
                              }
    _ -> do
      modifyState id

performAction _ _ _ = void do
  modifyState id

----------------------------------------------------------

_landingAction :: Prism' Action L.Action
_landingAction = prism LandingA \action ->
  case action of
    LandingA caction -> Right caction
    _-> Left action

_loginAction :: Prism' Action LN.Action
_loginAction = prism LoginA \action ->
  case action of
    LoginA caction -> Right caction
    _-> Left action

_addCorpusAction :: Prism' Action AC.Action
_addCorpusAction = prism AddCorpusA \action ->
  case action of
    AddCorpusA caction -> Right caction
    _-> Left action

_docViewAction :: Prism' Action DV.Action
_docViewAction = prism DocViewA \action ->
  case action of
    DocViewA caction -> Right caction
    _-> Left action

_searchAction :: Prism' Action S.Action
_searchAction = prism SearchA \action ->
  case action of
    SearchA caction -> Right caction
    _-> Left action

_userPageAction :: Prism' Action U.Action
_userPageAction = prism UserPageA \action ->
  case action of
    UserPageA caction -> Right caction
    _-> Left action

_dashBoardAction :: Prism' Action Dsh.Action
_dashBoardAction = prism DashboardA \action ->
  case action of
    DashboardA caction -> Right caction
    _ -> Left action

_docAnnotationViewAction :: Prism' Action D.Action
_docAnnotationViewAction = prism DocAnnotationViewA \action ->
  case action of
    DocAnnotationViewA caction -> Right caction
    _-> Left action

_treeAction :: Prism' Action Tree.Action
_treeAction = prism TreeViewA \action ->
  case action of
    TreeViewA caction -> Right caction
    _-> Left action

_tabviewAction :: Prism' Action TV.Action
_tabviewAction = prism TabViewA \action ->
  case action of
    TabViewA caction -> Right caction
    _-> Left action

_corpusAction :: Prism' Action CA.Action
_corpusAction = prism CorpusAnalysisA \action ->
  case action of
    CorpusAnalysisA caction -> Right caction
    _-> Left action

_graphExplorerAction :: Prism' Action GE.Action
_graphExplorerAction = prism GraphExplorerA \action ->
  case action of
    GraphExplorerA caction -> Right caction
    _-> Left action

_ngAction :: Prism' Action NG.Action
_ngAction = prism NgramsA \action ->
  case action of
    NgramsA caction -> Right caction
    _-> Left action



