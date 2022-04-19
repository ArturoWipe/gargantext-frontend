module Gargantext.Components.GraphExplorer.Frame.DocFocus
where
{-}
import Gargantext.Prelude

import Gargantext.Sessions (Session)
import Gargantext.Types (ListId, NodeID)
import Gargantext.Utils.Reactix as R
import Gargantext.Utils.Reactix as R2
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Frame.DocFocus"

type Props =
  ( listId    :: ListId
  , corpusId  :: NodeID
  , nodeId    :: NodeID
  , session   :: Session
  )

docFocus :: R2.Leaf Props
docFocus = R2.leaf docFocusCpt

docFocusCpt :: R.Component Props
docFocusCpt = here.component "main" cpt where
  cpt props _ = pure $ H.div {} []
