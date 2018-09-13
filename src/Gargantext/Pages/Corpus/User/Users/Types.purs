module Gargantext.Pages.Corpus.User.Users.Types
       (module Gargantext.Pages.Corpus.User.Users.Types.Types,
        module Gargantext.Pages.Corpus.User.Users.Types.Lens,
        module Gargantext.Pages.Corpus.User.Users.Types.States,
        brevetSpec,
        projectSpec,
        facets
       )
       where

import Prelude (($))

import Gargantext.Pages.Corpus.User.Users.Types.Lens
import Gargantext.Pages.Corpus.User.Users.Types.Types
import Gargantext.Pages.Corpus.User.Users.Types.States
import Gargantext.Pages.Corpus.User.Brevets as B
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Gargantext.Pages.Folder as PS
import Gargantext.Components.Tab (tabs)
import Thermite (Spec, focus)

brevetSpec :: Spec State {} Action
brevetSpec = focus _brevetslens _brevetsAction B.brevetsSpec

projectSpec :: Spec State {} Action
projectSpec = focus _projectslens _projectsAction PS.projets

facets :: Spec State {} Action
facets = tabs _tablens _tabAction $ fromFoldable
         [ Tuple "Publications (12)" publicationSpec
         , Tuple "Brevets (2)" brevetSpec
         , Tuple "Projets IMT (5)" projectSpec
         ]
