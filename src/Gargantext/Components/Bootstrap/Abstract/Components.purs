module Gargantext.Components.Bootstrap.Components
  ( Tree, tree, tree'
  , OptTree, optTree, optTree'
  , Leaf, leaf, leaf'
  , OptLeaf, optLeaf, optLeaf'
  ) where

import Gargantext.Prelude

import ConvertableOptions as CO
import Reactix as R

type Tree props
   = Record props
  -> Array R.Element
  -> R.Element

-- | UI Component type with only required props and children
tree :: forall props.
     String
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> Record props
  -> Array R.Element
  -> R.Element
tree name cpt
  = R.createElement $ R.hooksComponent name cpt

-- | Derived component `tree` creating a React Memo Component
tree' :: forall props.
     String
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> Record props
  -> Array R.Element
  -> R.Element
tree' name cpt
  = R.createElement $ R.memo' $ R.hooksComponent name cpt

-------------------------------------------------------

type OptTree options props provided
   = CO.Defaults (Record options) (Record provided) (Record props)
  => Record provided
  -> Array R.Element
  -> R.Element

-- | UI Component type containing optional props and children
optTree :: forall options provided props.
     CO.Defaults options provided (Record props)
  => String
  -> options
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> provided
  -> Array R.Element
  -> R.Element
optTree name options cpt props
  = R.createElement (R.hooksComponent name cpt) props'
    where
      props' = CO.defaults options props

-- | Derived component `optTree` creating a React Memo Component
optTree' :: forall options provided props.
     CO.Defaults options provided (Record props)
  => String
  -> options
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> provided
  -> Array R.Element
  -> R.Element
optTree' name options cpt props
  = R.createElement (R.memo' $ R.hooksComponent name cpt) props'
    where
      props' = CO.defaults options props

-------------------------------------------------------

type Leaf props
   = Record props
  -> R.Element

-- | UI Component type containing optional props and no child
leaf :: forall props.
     String
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> Record props
  -> R.Element
leaf name cpt props
  = R.createElement (R.hooksComponent name cpt) props []

-- | Derived component `leaf` creating a React Memo Component
leaf' :: forall props.
     String
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> Record props
  -> R.Element
leaf' name cpt props
  = R.createElement (R.memo' $ R.hooksComponent name cpt) props []

-------------------------------------------------------

type OptLeaf options props provided
   = CO.Defaults (Record options) (Record provided) (Record props)
  => Record provided
  -> R.Element

-- | UI Component type containing optional props and no child
optLeaf :: forall options provided props.
     CO.Defaults options provided (Record props)
  => String
  -> options
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> provided
  -> R.Element
optLeaf name options cpt props
  = R.createElement (R.hooksComponent name cpt) props' []
    where
      props' = CO.defaults options props

-- | Derived component `optLeaf` creating a React Memo Component
optLeaf' :: forall options provided props.
     CO.Defaults options provided (Record props)
  => String
  -> options
  -> (Record props -> Array R.Element -> R.Hooks R.Element)
  -> provided
  -> R.Element
optLeaf' name options cpt props
  = R.createElement (R.memo' $ R.hooksComponent name cpt) props' []
    where
      props' = CO.defaults options props
