module Gargantext.Components.Lang where

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Gargantext.Prelude (class Eq, class Show, show, class Read)

-- Language used for search
allLangs :: Array Lang
allLangs = [ EN
           , FR
           , Universal
           , No_extraction
           ]

data Lang = FR | EN | Universal | No_extraction

instance Show Lang where
  show FR            = "FR"
  show EN            = "EN"
  show Universal     = "All"
  show No_extraction = "Nothing"

derive instance Eq Lang

instance Read Lang where
  read "FR"      = Just FR
  read "EN"      = Just EN
  read "All"     = Just Universal
  read "Nothing" = Just No_extraction
  read _         = Nothing


instance EncodeJson Lang where
  encodeJson a = encodeJson (show a)

-- Language used for the landing page
data LandingLang = LL_EN | LL_FR

-- @TODO a possible method/class that a real i18n logic could later replace
class Show t <= Translate t where
  translate :: Lang -> t -> String
