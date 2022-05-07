module Gargantext.Components.Document.Layout
  ( layout
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Annotation.Field as AnnotatedField
import Gargantext.Components.AutoUpdate (autoUpdate)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Document.Types (DocPath, Document(..), LoadedData, initialState)
import Gargantext.Components.NgramsTable.Core (CoreAction(..), Versioned(..), addNewNgramA, applyNgramsPatches, coreDispatch, replace, setTermListA, syncResetButtons, findNgramRoot)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Utils (nbsp)
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( loaded   :: LoadedData
  , path     :: DocPath
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Document.layout"

layout :: R2.Leaf Props
layout = R2.leaf layoutCpt

layoutCpt :: R.Component Props
layoutCpt = here.component "main" cpt where
  -- Component
  cpt { path
      , loaded:
          loaded@{ ngramsTable: Versioned
          { data: initTable }
          , document: NodePoly
            { hyperdata: Document doc
            }
          }
      } _ = do
    -- | States
    -- |

    state'@{ ngramsLocalPatch } /\ state <-
      R2.useBox' $ initialState { loaded }

    -- | Computed
    -- |
    let

      dispatch = coreDispatch path state

      afterSync = \_ -> pure unit

      withAutoUpdate = false

      ngrams = applyNgramsPatches state' initTable

      annotate text = AnnotatedField.annotatedField
        { ngrams
        , setTermList
        , text
        }

      setTermListOrAddA ngram Nothing        =
        addNewNgramA ngram
      setTermListOrAddA ngram (Just oldList) =
        setTermListA ngram <<< replace oldList

      setTermList ngram mOldList =
        dispatch <<< setTermListOrAddA (findNgramRoot ngrams ngram) mOldList

      hasPublication = (isJust doc.source) || (isJust doc.publication_date)

      hasAbstract =  maybe false (not String.null) doc.abstract

    -- | Render
    -- |
    pure $

      H.div
      { className: "document-layout" }
      -- @WIP
      --
      -- <>
      --DEBUG
      --[ H.pre { rows: 30 } [
      --    H.text (stringifyWithIndent 2 (encodeJson (fst state)))
      --  ] ] <>
      [
        H.div
        { className: "document-layout__controls" }
        [
          R2.if' withAutoUpdate $

            autoUpdate
            { duration: 5000
            , effect: dispatch $ Synchronize { afterSync }
            }
        ,
          syncResetButtons
          { afterSync
          , ngramsLocalPatch
          , performAction: dispatch
          }
        ]
      ,
        H.div
        { className: "document-layout__title" }
        [
          annotate doc.title
        ]
      ,
        R2.fromMaybe_ doc.authors \authors ->

          H.div
          { className: "document-layout__authors" }
          [
            -- TODO add href to /author/ if author present in
            annotate (Just authors)
          ]
      ,
        R2.if' hasPublication $

          H.div
          { className: "document-layout__publication" }
          [
            R2.fromMaybe_ doc.source \source ->

              H.text $ source <> "," <> nbsp 1
          ,
            H.text $ publicationDate $ Document doc
          ]
      ,
        R2.if' hasAbstract $

          H.div
          { className: "document-layout__abstract" }
          [
            B.div'
            { className: "document-layout__abstract__title" }
            "Abstract"
          ,
            H.div
            { className: "document-layout__abstract__content" }
            [
              annotate doc.abstract
            ]
          ]
      -- (?) remove "Full text" block (unused feature for now,
      --     see #334)
      -- , H.div { className: "jumbotron" } [ H.p {} [ H.text "Empty Full Text" ] ]
      ]


-------------------------------------------------------------

publicationDate :: Document -> String
publicationDate (Document {publication_year: Nothing}) = ""
publicationDate (Document {publication_year: Just py, publication_month: Nothing}) = U.zeroPad 2 py
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Nothing}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm)
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Just pd}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm) <> "-" <> (U.zeroPad 2 pd)
