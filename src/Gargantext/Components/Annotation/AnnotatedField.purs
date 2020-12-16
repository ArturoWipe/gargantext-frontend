-- | The AnnotatedField Component is for colouring ngrams that appear in a text
-- | 
-- | Given an array of ngrams and a text, it:
-- | 
-- | 1. Searches the text for the ngrams
-- | 2. Renders each the resulting runs according to the Maybe TermList they appear in
-- | 
-- | Notes:
-- | 
-- | 1. We must only re-search the text when the ngrams change for performance
-- | 2. We will need a more ambitious search algorithm for skipgrams.
module Gargantext.Components.Annotation.AnnotatedField where

import Data.List ( List(..), (:) )
import Data.Maybe ( Maybe(..), maybe )
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ( (/\) )
--import DOM.Simple.Console (log2)
import DOM.Simple.Event as DE
import Effect ( Effect )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Prelude

import Gargantext.Types (CTabNgramType(..), TermList)
import Gargantext.Components.Annotation.Utils ( termBootstrapClass )
import Gargantext.Components.NgramsTable.Core (NgramsTable, NgramsTerm, findNgramTermList, highlightNgrams, normNgram)
import Gargantext.Components.Annotation.Menu ( annotationMenu, MenuType(..) )
import Gargantext.Utils.Selection as Sel

thisModule :: String
thisModule = "Gargantext.Components.Annotation.AnnotatedField"

type Props =
  ( ngrams       :: NgramsTable
  , setTermList  :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit
  , text         :: Maybe String
  )
type MouseEvent = E.SyntheticEvent DE.MouseEvent

-- UNUSED
-- defaultProps :: Record Props
-- defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing, setTermList: \_ _ _ -> pure unit }

annotatedField :: Record Props -> R.Element
annotatedField p = R.createElement annotatedFieldComponent p []

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = R.hooksComponentWithModule thisModule "annotatedField" cpt
  where
    cpt {ngrams,setTermList,text: fieldText} _ = do
      (_ /\ setRedrawMenu) <- R.useState' false

      menuRef <- R.useRef Nothing

      let wrapperProps = { className: "annotated-field-wrapper" }

          redrawMenu = setRedrawMenu not

          hideMenu = do
            R.setRef menuRef Nothing
            redrawMenu

          showMenu { event, getList, menuType, ngram } = do
            let x = E.clientX event
                y = E.clientY event
                -- n = normNgram CTabTerms text
                list = getList ngram
                setList t = do
                  setTermList ngram list t
                  hideMenu
            E.preventDefault event
            --range <- Sel.getRange sel 0
            --log2 "[showMenu] selection range" $ Sel.rangeToTuple range
            let menu = Just
                  { x
                  , y
                  , list
                  , menuType
                  , onClose: hideMenu
                  , setList
                  }
            R.setRef menuRef menu
            redrawMenu

          onSelect :: Maybe (Tuple NgramsTerm TermList) -> MouseEvent -> Effect Unit
          onSelect (Just (Tuple ngram list)) event =
            showMenu { event, getList: const (Just list), menuType: SetTermListItem, ngram }
          onSelect Nothing event = do
            s <- Sel.getSelection
            case s of
              Just sel -> do
                case Sel.selectionToString sel of
                  "" -> hideMenu
                  sel' -> do
                    showMenu { event, getList: findNgramTermList ngrams, menuType: NewNgram, ngram: normNgram CTabTerms sel' }
              Nothing -> hideMenu

          wrap (text /\ list) = {text, list, onSelect}

      pure $ HTML.div wrapperProps
        [ maybe (HTML.div {} []) annotationMenu $ R.readRef menuRef
        , HTML.div { className: "annotated-field-runs" }
             $ annotateRun
            <$> wrap
            <$> compile ngrams fieldText
        ]

compile :: NgramsTable -> Maybe String -> Array (Tuple String (List (Tuple NgramsTerm TermList)))
compile ngrams = maybe [] (highlightNgrams CTabTerms ngrams)

-- Runs

type Run =
  ( list :: List (Tuple NgramsTerm TermList)
  , onSelect :: Maybe (Tuple NgramsTerm TermList) -> MouseEvent -> Effect Unit
  , text :: String
  )

annotateRun :: Record Run -> R.Element
annotateRun p = R.createElement annotatedRunComponent p []

annotatedRunComponent :: R.Component Run
annotatedRunComponent = R.staticComponent "AnnotatedRun" cpt
  where
    cpt    { list: Nil, onSelect, text }     _ =
      HTML.span { on: { mouseUp: onSelect Nothing } } [ HTML.text text ]

    cpt    { list: (ngram /\ list) : _otherLists, onSelect, text } _ =
      -- TODO _otherLists
      HTML.span { className
                , on: { click: onSelect (Just (ngram /\ list)) } } [ HTML.text text ]
      where
        className = "annotation-run bg-" <> termBootstrapClass list

