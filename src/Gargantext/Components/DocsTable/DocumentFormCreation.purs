module Gargantext.Components.DocsTable.DocumentFormCreation
  ( documentFormCreation
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log, log3)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Hooks.FormValidation (useFormValidation)
import Gargantext.Hooks.FormValidation.Types (VForm)
import Gargantext.Hooks.FormValidation.Unboxed as FV
import Gargantext.Hooks.StateRecord (useStateRecord)
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record (merge)


type DocumentFormData =
  ( title     :: String
  , source    :: String
  , authors   :: String
  , abstract  :: String
  )

documentDefaultData :: Record DocumentFormData
documentDefaultData =
  { title     : ""
  , source    : ""
  , authors   : ""
  , abstract  : ""
  }

documentFormCreation :: R2.Leaf ()
documentFormCreation = R2.leaf documentFormCreationCpt
documentFormCreationCpt :: R.Component ()
documentFormCreationCpt = R.hooksComponent "documentFormCreation" cpt where
  cpt _ _ = do
    -- Hooks
    { state, setStateKey, bindStateKey } <- useStateRecord documentDefaultData
    fv <- useFormValidation

    -- @onSubmit: exec whole form validation and execute callback
    onSubmit <- pure $ do

      result <- fv.try (\_ -> documentFormValidation state)

      case result of
        Left err -> log3 "document form error" state err
        Right _  -> log "ok"

    -- Render
    pure $

      H.form
      { className: "document-form-creation" }
      [
        -- Title
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "title") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div { className: "form-group__label" }
          [
            H.label {} [ H.text "Title" ]
          ]
        ,
          H.div { className: "form-group__field" }
          [
            B.formInput $
              bindStateKey "title"
          ,
            R2.if' (fv.hasError' "title") $
              H.div { className: "form-group__error" }
              [ H.text "Please enter a title" ]
          ]
        ]
      ,
        -- Source
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "source") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div { className: "form-group__label" }
          [
            H.label {} [ H.text "Source" ]
          ]
        ,
          H.div { className: "form-group__field" }
          [
            B.formInput $
              bindStateKey "source"
          ,
            R2.if' (fv.hasError' "source") $
              H.div { className: "form-group__error" }
              [ H.text "Please enter a source" ]
          ]
        ]
      ,
        -- Authors
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "authors") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div { className: "form-group__label" }
          [
            H.label {} [ H.text "Authors" ]
          ]
        ,
          H.div { className: "form-group__field" }
          [
            B.formInput $
            { placeholder: "ex: author1, author2, …"
            } `merge` bindStateKey "authors"
          ,
            R2.if' (fv.hasError' "authors") $
              H.div { className: "form-group__error" }
              [ H.text "Please enter at least one author" ]
          ]
        ]
      ,
        -- Abstract
        H.div
        { className: intercalate " "
            [ "form-group"
            ]
        }
        [
          H.div { className: "form-group__label" }
          [
            H.label {} [ H.text $ "Abstract" <> nbsp 1 ]
          ,
            H.span
            { className: "form-group__label--sub" }
            [ H.text "optional" ]
          ]
        ,
          H.div { className: "form-group__field" }
          [
            B.formTextarea $
            bindStateKey "abstract"
          ]
        ]
      ,
        -- Submit
        H.div { className: "document-form-creation__submit" }
        [
          B.button
          { callback: \_ -> onSubmit
          -- , status: props.status == "deferred" ? "deferred" $ "enabled"
          , variant: "primary"
          , type: "submit"
          , block: true
          }
          [ H.text "Add" ]
        ]
      ]

documentFormValidation :: Record DocumentFormData -> Effect VForm
documentFormValidation r = foldl append mempty rules
  where
    rules =
      [ FV.nonEmpty "title" r.title
      , FV.nonEmpty "source" r.source
      , FV.nonEmpty "authors" r.authors
      ]
