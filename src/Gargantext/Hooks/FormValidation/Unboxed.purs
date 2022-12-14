module Gargantext.Hooks.FormValidation.Unboxed
  ( class Equals, equals
  , class NonEmpty, nonEmpty
  , class Minimum, minimum
  , class Maximum, maximum
  , lowercase, uppercase, email, date
  ) where

import Gargantext.Prelude

import Data.String (toLower, toUpper)
import Data.String.CodeUnits (length)
import Data.String.Regex (test)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (invalid)
import Effect (Effect)
import Gargantext.Hooks.FormValidation.Types (Field, VForm, emailPattern, datePattern)

class Eq a <= Equals a where
  equals :: Field -> a -> a -> Effect VForm

class NonEmpty a where
  nonEmpty :: Field -> a -> Effect VForm

class Ord a <= Minimum a where
  minimum :: Field -> a -> Int -> Effect VForm

class Ord a <= Maximum a where
  maximum :: Field -> a -> Int -> Effect VForm

-- Regarding String field value

instance equalsString :: Equals String where
  equals field input input'
    | (not eq input input') = pure $ invalid [ field /\ "equals" ]
    | otherwise             = pure $ pure unit

instance nonEmptyString :: NonEmpty String where
  nonEmpty field "" = pure $ invalid [ field /\ "nonEmpty" ]
  nonEmpty _ _      = pure $ pure unit

instance minimumString :: Minimum String where
  minimum field input min
    | (length input) < min = pure $ invalid [ field /\ "minimum" ]
    | otherwise            = pure $ pure unit

instance maximumString :: Maximum String where
  maximum field input max
    | (length input) > max = pure $ invalid [ field /\ "maximum" ]
    | otherwise            = pure $ pure unit

uppercase :: Field -> String -> Effect VForm
uppercase field input
  | (toLower input) == input = pure $ invalid [ field /\ "uppercase" ]
  | otherwise                = pure $ pure unit

lowercase :: Field -> String -> Effect VForm
lowercase field input
  | (toUpper input) == input = pure $ invalid [ field /\ "lowercase" ]
  | otherwise                = pure $ pure unit

email :: Field -> String -> Effect VForm
email field input
  | (not $ test emailPattern input) = pure $ invalid [ field /\ "email" ]
  | otherwise                       = pure $ pure unit

date :: Field -> String -> Effect VForm
date field input
  | (not $ test datePattern input) = pure $ invalid [ field /\ "date" ]
  | otherwise                      = pure $ pure unit
