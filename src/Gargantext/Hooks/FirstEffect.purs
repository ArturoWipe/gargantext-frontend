module Gargantext.Hooks.FirstEffect
  ( useFirstEffect, useFirstEffect'
  ) where

import Gargantext.Prelude

import Effect (Effect)
import Gargantext.Hooks.FirstMount (useFirstMount)
import Reactix (nothing, thenNothing)
import Reactix as R

-- | Hook triggered on first mount only
useFirstEffect :: Effect (Effect Unit) -> R.Hooks Unit
useFirstEffect e = useFirstMount >>= eff e

-- | Like `useFirstEffect` but Effect fn does return a cleanup handler
useFirstEffect' :: forall a. Effect a -> R.Hooks Unit
useFirstEffect' e = useFirstMount >>= eff (e # thenNothing)

eff :: Effect (Effect Unit) -> Boolean -> R.Hooks Unit
eff e b = R.useEffect if b then e else nothing # thenNothing
