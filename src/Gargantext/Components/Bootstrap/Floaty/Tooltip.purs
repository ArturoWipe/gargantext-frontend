module Gargantext.Components.Bootstrap.Tooltip
  ( tooltip
  , tooltipBind
  ) where

import Gargantext.Prelude

import ConvertableOptions as CO
import Data.Symbol (SProxy(..))
import Gargantext.Components.Bootstrap.Types (TooltipEffect(..), Variant(..))
import Reactix as R
import Record as Record

foreign import reactTooltipCpt :: R.Component Props

type Props =
  ( id          :: String
  | Options
  )

type Options =
  ( effect      :: TooltipEffect
  , variant     :: Variant
  , delayHide   :: Int
  , delayShow   :: Int
  , className   :: String
  )

options :: Record Options
options =
  { effect      : SolidEffect
  , variant     : Dark
  , delayHide   : 0
  , delayShow   : 0
  , className   : ""
  }


-- | Adapter Component for React Tooltip
-- |
-- | https://github.com/wwayne/react-tooltip
tooltip :: forall provided.
     CO.Defaults (Record Options) (Record provided) (Record Props)
  => Record provided
  -> Array R.Element
  -> R.Element
tooltip props = R.rawCreateElement reactTooltipCpt props''
  where
    props'  = CO.defaults options props
    props'' = props'
        # Record.set
          (SProxy :: SProxy "effect")
          (show props'.effect)
      >>> Record.set
          (SProxy :: SProxy "variant")
          (show props'.variant)
      >>> Record.rename
          (SProxy :: SProxy "variant")
          (SProxy :: SProxy "type")

-------------------------------------------------------------

type TooltipBindingProps =
  ( "data-tip" :: Boolean
  , "data-for" :: String
  )

-- | Bind a Component props to an existing <tooltip>
tooltipBind :: String -> Record TooltipBindingProps
tooltipBind =
  { "data-for": _
  , "data-tip": true
  }
