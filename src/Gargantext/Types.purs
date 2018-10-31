module Gargantext.Types where

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe(..))
import Gargantext.Prelude

data TermType = MonoTerm | MultiTerm

derive instance eqTermType :: Eq TermType

instance showTermType :: Show TermType where
  show MonoTerm  = "MonoTerm"
  show MultiTerm = "MultiTerm"

readTermType :: String -> Maybe TermType
readTermType "MonoTerm"  = Just MonoTerm
readTermType "MultiTerm" = Just MultiTerm
readTermType _           = Nothing

termTypes :: Array { desc :: String, mval :: Maybe TermType }
termTypes = [ { desc: "All types",        mval: Nothing        }
            , { desc: "One-word terms",   mval: Just MonoTerm  }
            , { desc: "Multi-word terms", mval: Just MultiTerm }
            ]

data TermList = GraphTerm | StopTerm | CandidateTerm

derive instance eqTermList :: Eq TermList

instance decodeJsonTermList :: DecodeJson TermList where
  decodeJson json = pure GraphTerm  -- TODO

type ListTypeId = Int

listTypeId :: TermList -> ListTypeId
listTypeId GraphTerm     = 1
listTypeId StopTerm      = 2
listTypeId CandidateTerm = 3

instance showTermList :: Show TermList where
  show GraphTerm     = "Graph"
  show StopTerm      = "Stop"
  show CandidateTerm = "Candidate"

readTermList :: String -> Maybe TermList
readTermList "Graph"     = Just GraphTerm
readTermList "Stop"      = Just StopTerm
readTermList "Candidate" = Just CandidateTerm
readTermList _           = Nothing

termLists :: Array { desc :: String, mval :: Maybe TermList }
termLists = [ { desc: "All terms",   mval: Nothing      }
            , { desc: "Graph terms",   mval: Just GraphTerm   }
            , { desc: "Stop terms",  mval: Just StopTerm  }
            , { desc: "Candidate terms", mval: Just CandidateTerm }
            ]

