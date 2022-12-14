module Gargantext.Hooks.FormValidation.Types
  ( VForm, EForm, Field
  , emailPattern, datePattern
  ) where

import Gargantext.Prelude

import Data.Either (Either)
import Data.String.Regex (Regex)
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V)

foreign import emailPattern :: Regex
foreign import datePattern :: Regex

-- @TODO: types for errors (`Tuple Field String`)?

type Field = String

type EForm = Either (Array (Tuple Field String)) Unit
type VForm = V      (Array (Tuple Field String)) Unit
