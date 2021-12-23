module Gargantext.Hooks.FirstMount
  ( useFirstMount
  ) where

import Gargantext.Prelude

import Reactix (nothing)
import Reactix as R

-- | Hook triggered on first mount event only
useFirstMount :: R.Hooks (Boolean)
useFirstMount = do
  firstMount <- R.useRef true

  let firstMount' = R.readRef firstMount

  R.unsafeHooksEffect
    if firstMount' == true
    then R.setRef firstMount false
    else nothing

  pure firstMount'
