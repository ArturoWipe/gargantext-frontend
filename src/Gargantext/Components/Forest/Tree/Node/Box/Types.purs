module Gargantext.Components.Forest.Tree.Node.Box.Types where

import DOM.Simple as DOM
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction)
import Gargantext.Prelude (Unit)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID, Name)
import Gargantext.Types as GT

type CommonProps =
  ( dispatch :: Action -> Aff Unit
  , session  :: Session
  )

type NodePopupProps =
  ( boxes :: Boxes
  , id             :: ID
  , name           :: Name
  , nodeType       :: GT.NodeType
  , onPopoverClose :: DOM.Element -> Effect Unit
  | CommonProps
  )

type NodePopupS =
  ( action   :: Maybe NodeAction
  , id       :: ID
  , name     :: Name
  , nodeType :: GT.NodeType
  )


