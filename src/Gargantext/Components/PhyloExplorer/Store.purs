module Gargantext.Components.PhyloExplorer.Store
  ( Store
  , State
  , options
  , context
  , provide
  , use
  ) where


import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Gargantext.Components.PhyloExplorer.Types (DisplayView(..), ExtractedCount, ExtractedTerm, FrameDoc, PhyloDataSet, Source, TabView(..), Term)
import Gargantext.Types (NodeID, SidePanelState(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Stores as Stores
import Reactix as R
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Store"

type Store =
  -- Data
  ( phyloDataSet       :: T.Box PhyloDataSet
  , phyloId            :: T.Box NodeID
  , isBuilt            :: T.Box Boolean
  -- Layout
  , toolBarDisplayed   :: T.Box Boolean
  , isIsolineDisplayed :: T.Box Boolean
  , sideBarDisplayed   :: T.Box SidePanelState
  , sideBarTabView     :: T.Box TabView
  , frameDoc           :: T.Box (Maybe FrameDoc)
  -- Topbar
  , source             :: T.Box String
  , sources            :: T.Box (Array Source)
  , terms              :: T.Box (Array Term)
  , search             :: T.Box String
  , result             :: T.Box (Maybe Term)
  -- Sidebar
  , extractedTerms     :: T.Box (Array ExtractedTerm)
  , selectedTerm       :: T.Box (Maybe String)
  , selectedBranch     :: T.Box (Maybe String)
  , selectedSource     :: T.Box (Maybe String)
  , extractedCount     :: T.Box (Maybe ExtractedCount)
  -- Toolbar
  , displayView        :: T.Box DisplayView
  )

type State =
  -- Data
  ( phyloDataSet       :: PhyloDataSet
  , phyloId            :: NodeID
  , isBuilt            :: Boolean
  -- Layout
  , toolBarDisplayed   :: Boolean
  , isIsolineDisplayed :: Boolean
  , sideBarDisplayed   :: SidePanelState
  , sideBarTabView     :: TabView
  , frameDoc           :: Maybe FrameDoc
  -- Topbar
  , source             :: String
  , sources            :: Array Source
  , terms              :: Array Term
  , search             :: String
  , result             :: Maybe Term
  -- Sidebar
  , extractedTerms     :: Array ExtractedTerm
  , selectedTerm       :: Maybe String
  , selectedBranch     :: Maybe String
  , selectedSource     :: Maybe String
  , extractedCount     :: Maybe ExtractedCount
  -- Toolbar
  , displayView        :: DisplayView
  )

options ::
  { isBuilt            :: Boolean
  -- Layout
  , toolBarDisplayed   :: Boolean
  , isIsolineDisplayed :: Boolean
  , sideBarDisplayed   :: SidePanelState
  , sideBarTabView     :: TabView
  , frameDoc           :: Maybe FrameDoc
  -- Topbar
  , source             :: String
  , sources            :: Array Source
  , terms              :: Array Term
  , search             :: String
  , result             :: Maybe Term
  -- Sidebar
  , extractedTerms     :: Array ExtractedTerm
  , selectedTerm       :: Maybe String
  , selectedBranch     :: Maybe String
  , selectedSource     :: Maybe String
  , extractedCount     :: Maybe ExtractedCount
  -- Toolbar
  , displayView        :: DisplayView
  }
options =
  -- Data
  { isBuilt            : false
  -- Layout
  , toolBarDisplayed   : false
  , isIsolineDisplayed : false
  , sideBarDisplayed   : InitialClosed
  , sideBarTabView     : DetailsTab
  , frameDoc           : Nothing
  -- Topbar
  , source             : ""
  , sources            : mempty
  , terms              : mempty
  , search             : ""
  , result             : Nothing
  -- Sidebar
  , extractedTerms     : mempty
  , selectedTerm       : Nothing
  , selectedBranch     : Nothing
  , selectedSource     : Nothing
  , extractedCount     : Nothing
  -- Toolbar
  , displayView        : HeadingMode
  }

context :: R.Context (Record Store)
context = R.createContext $ unsafeCoerce unit

provide :: Record State -> Array R.Element -> R.Element
provide values = Stores.provideStore here.name values context

use :: R.Hooks (Record Store)
use = Stores.useStore context
