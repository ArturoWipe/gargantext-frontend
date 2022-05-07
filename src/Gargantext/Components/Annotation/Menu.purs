-- | A ContextMenU that allows you to add terms to a list
module Gargantext.Components.Annotation.Menu where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.Annotation.Types (MenuType(..), termClass)
import Gargantext.Components.Bootstrap as B
import Gargantext.Types (TermList(..), termListName)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Annotation.Menu"

type Props =
  ( list     :: Maybe TermList
  , menuType :: MenuType
  , setList  :: TermList -> Effect Unit -- not a state hook setter
  )

type AnnotationMenu =
  ( closeCallback :: Unit -> Effect Unit
  , redrawMenu    :: T.Box Boolean
  , x             :: Number
  , y             :: Number
  | Props
  )

type AnnotationMenuWrapper =
  ( menuRef :: R.Ref (Maybe (Record AnnotationMenu))
  )

eqAnnotationMenu :: Record AnnotationMenu -> Record AnnotationMenu -> Boolean
eqAnnotationMenu new old = new.list == old.list &&
                           new.menuType == old.menuType &&
                           new.x == old.x &&
                           new.y == old.y

eqAnnotationMenuWrapper :: { new :: Maybe (Record AnnotationMenu)
                           , old :: Maybe (Record AnnotationMenu) } -> Effect Boolean
eqAnnotationMenuWrapper { new: Nothing, old: Nothing } = pure $ true
eqAnnotationMenuWrapper { new: Nothing, old: Just _ } = pure $ false
eqAnnotationMenuWrapper { new: Just _, old: Nothing } = pure $ false
eqAnnotationMenuWrapper { new: Just n, old: Just o } = pure $ eqAnnotationMenu n o

annotationMenuWrapper :: R2.Leaf AnnotationMenuWrapper
annotationMenuWrapper = R2.leafComponent annotationMenuWrapperCpt
annotationMenuWrapperCpt :: R.Component AnnotationMenuWrapper
annotationMenuWrapperCpt = here.component "annotationMenuWrapper" cpt where
  cpt { menuRef } _ = pure $ R2.fromMaybe_ (R.readRef menuRef) annotationMenu

-- | An Annotation Menu is parameterised by a Maybe Termlist of the
-- | TermList the currently selected text belongs to
annotationMenu :: R2.Leaf AnnotationMenu
annotationMenu = R2.leafComponent annotationMenuCpt
annotationMenuCpt :: R.Component AnnotationMenu
annotationMenuCpt = here.component "annotationMenu" cpt where
  cpt { x, y, list, menuType, closeCallback, setList } _ = do
    -- redrawMenu' <- T.useLive T.unequal redrawMenu

    pure $
      B.contextMenu
      { x
      , y
      , closeCallback
      }
      [
        annotationMenuInner { list, menuType, setList }
      ]

annotationMenuInner :: R2.Leaf Props
annotationMenuInner = R2.leafComponent annotationMenuInnerCpt
annotationMenuInnerCpt :: R.Component Props
annotationMenuInnerCpt = here.component "annotationMenuInner" cpt where
  cpt props _ = pure $ R.fragment $

    A.mapMaybe (addToList props) [ MapTerm, CandidateTerm, StopTerm ]

-- | Given the TermList to render the item for zand the Maybe TermList the item may belong to, possibly render the menuItem
addToList :: Record Props -> TermList -> Maybe R.Element
addToList {list: Just t'} t
  | t == t'   = Nothing

addToList {menuType, setList} t = Just $
  B.contextMenuItem
  { callback: click }
  [
    B.icon
    { name: "pencil-square"
    , className: "mr-1 " <> termClass t
    }
  ,
    H.text (label menuType)
  ]

  where
    label NewNgram = "Add to " <> termListName t
    label SetTermListItem = "Change to " <> termListName t
    click _ = setList t
