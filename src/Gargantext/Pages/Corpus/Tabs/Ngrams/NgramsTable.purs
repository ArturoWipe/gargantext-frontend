module Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable
  (Mode(..), ngramsTableSpec)
  where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import React (ReactElement)
import React as React
import React.DOM hiding (style, map)
import React.DOM.Props (_id, _type, checked, className, href, name, onChange, onClick, onInput, placeholder, scope, selected, style, value)
import React.DOM.Props as DOM
import Thermite (Render, Spec, createClass, defaultPerformAction, simpleSpec)

import Gargantext.Types
import Gargantext.Components.Node (NodePoly)
import Gargantext.Components.NgramsTable as NT
import Gargantext.Prelude
import Gargantext.Config (CTabNgramType(..), End(..), Path(..), TabSubType(..), TabType(..), toUrl)
import Gargantext.Config.REST (get)
import Gargantext.Components.Loader as Loader
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo)

data Mode = Authors | Sources | Institutes | Terms

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

type Props = NT.Props (NodePoly CorpusInfo) Mode

type PageParams = NT.PageParams Mode

getTable :: CTabNgramType -> Maybe Int -> Aff NT.NgramsTable
getTable tab = get <<< toUrl Back (Ngrams (TabCorpus (TabNgramType tab)) Nothing)

modeTabType :: Mode -> CTabNgramType
modeTabType Authors = CTabAuthors
modeTabType Sources = CTabSources
modeTabType Institutes = CTabInstitutes
modeTabType Terms = CTabTerms

loadPage :: PageParams -> Aff NT.NgramsTable
loadPage {nodeId, mode} = getTable (modeTabType mode) (Just nodeId) -- TODO this ignores params

ngramsLoaderClass :: Loader.LoaderClass PageParams NT.NgramsTable
ngramsLoaderClass = Loader.createLoaderClass "CorpusNgramsLoader" loadPage

ngramsLoader :: Loader.Props' PageParams NT.NgramsTable -> ReactElement
ngramsLoader props = React.createElement ngramsLoaderClass props []

ngramsTableClass :: Loader.InnerClass PageParams NT.NgramsTable
ngramsTableClass = createClass "CorpusNgramsTable" NT.ngramsTableSpec NT.initialState

ngramsTableSpec :: Spec {} Props Void
ngramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId, mode} _ _ =
      -- TODO: ignored loaded param
      [ ngramsLoader { path: NT.initialPageParams nodeId mode
                     , component: ngramsTableClass
                     } ]
