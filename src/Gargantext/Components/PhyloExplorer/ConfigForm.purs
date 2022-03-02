module Gargantext.Components.PhyloExplorer.ConfigForm
  ( configForm
  , FormData
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log3)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.PhyloExplorer.API (CliqueFilter(..), ReflexiveClique(..), ReflexiveTimeUnit(..))
import Gargantext.Hooks.FormValidation (useFormValidation)
import Gargantext.Hooks.StateRecord (useStateRecord)
import Gargantext.Hooks.StateRecord.Behaviors (setter)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record (merge)
import Record as Record
import Record.Extra (pick)

type Props =
  ( callback  :: Record FormData -> Effect Unit
  , status    :: ComponentStatus
  | Options
  )

type Options = ( | FormData )

options :: Record Options
options = Record.merge {} defaultData

configForm :: forall r. R2.OptLeaf Options Props r
configForm = R2.optLeaf component options

component :: R.Component Props
component = R.hooksComponent "configForm" cpt where
  cpt props _ = do
  -- Hooks

    { state
    , bindStateKey
    , stateBox
    } <- useStateRecord (pick props :: Record FormData)

    fv <- useFormValidation

  -- Behaviors
    let

      -- @onSubmit: exec whole form validation and execute callback
      onSubmit = do

        -- result <- fv.try (\_ -> formValidation state)
        result <- pure $ Right state
        case result of
          Left err -> log3 "configForm validation error" state err
          Right _  -> props.callback state

  -- Render

    pure $

      H.form
      { className: "document-form-creation" }
      [
        -- Proximity
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "proximity") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div
          { className: "form-group__label" }
          [
            H.label {} [ H.text "Proximity" ]
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formInput $
            { type: "number"
            } `merge` bindStateKey "proximity"
          ,
            R2.if' (fv.hasError' "proximity") $
              H.div
              { className: "form-group__error" }
              [
                H.text "Please enter a `Double` value (eg. 0.5)"
              ]
          ]
        ]
      ,
        -- Synchrony
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "synchrony") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div
          { className: "form-group__label" }
          [
            H.label {} [ H.text "Synchrony" ]
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formInput $
            { type: "number"
            } `merge` bindStateKey "synchrony"
          ,
            R2.if' (fv.hasError' "synchrony") $
              H.div
              { className: "form-group__error" }
              [
                H.text "Please enter a `Double` value (eg. 0.5)"
              ]
          ]
        ]
      ,
        -- Quality
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "quality") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div
          { className: "form-group__label" }
          [
            H.label {} [ H.text "Quality" ]
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formInput $
            { type: "number"
            } `merge` bindStateKey "quality"
          ,
            R2.if' (fv.hasError' "quality") $
              H.div
              { className: "form-group__error" }
              [
                H.text "Please enter a `Double` value (eg. 0.5)"
              ]
          ]
        ]
      ,
        -- Export filter
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "exportFilter") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div
          { className: "form-group__label" }
          [
            H.label {} [ H.text "Export filter" ]
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formInput $
            { type: "number"
            } `merge` bindStateKey "exportFilter"
          ,
            R2.if' (fv.hasError' "exportFilter") $
              H.div
              { className: "form-group__error" }
              [
                H.text "Please enter a `Double` value (eg. 0.5)"
              ]
          ]
        ]
      ,
        -- Time Unit
        B.fieldset
        { titleSlot: H.text "Time unit" }
        [
          -- Granularity
          H.div
          { className: intercalate " "
              [ "form-group"
              ]
          }
          [
            H.div
            { className: "form-group__label" }
            [
              H.label {} [ H.text "Granularity" ]
            ]
          ,
            H.div
            { className: "form-group__field" }
            [
              B.formSelect
              (bindStateKey "granularity")
              [
                H.option
                { value: show Year_ }
                [ H.text "Year" ]
              ,
                H.option
                { value: show Month_ }
                [ H.text "Month" ]
              ,
                H.option
                { value: show Week_ }
                [ H.text "Week" ]
              ,
                H.option
                { value: show Day_ }
                [ H.text "Day" ]
              ]
            ]
          ]
        ,
          -- Period
          H.div
          { className: intercalate " "
              [ "form-group"
              , (fv.hasError' "period") ?
                  "form-group--error" $
                  mempty
              ]
          }
          [
            H.div
            { className: "form-group__label" }
            [
              H.label {} [ H.text "Period" ]
            ]
          ,
            H.div
            { className: "form-group__field" }
            [
              B.formInput $
              { type: "number"
              } `merge` bindStateKey "period"
            ,
              R2.if' (fv.hasError' "period") $
                H.div
                { className: "form-group__error" }
                [
                  H.text "Please enter an `Int` value (eg. 3)"
                ]
            ]
          ]
        ,
          -- Step
          H.div
          { className: intercalate " "
              [ "form-group"
              , (fv.hasError' "step") ?
                  "form-group--error" $
                  mempty
              ]
          }
          [
            H.div
            { className: "form-group__label" }
            [
              H.label {} [ H.text "Step" ]
            ]
          ,
            H.div
            { className: "form-group__field" }
            [
              B.formInput $
              { type: "number"
              } `merge` bindStateKey "step"
            ,
              R2.if' (fv.hasError' "step") $
                H.div
                { className: "form-group__error" }
                [
                  H.text "Please enter an `Int` value (eg. 3)"
                ]
            ]
          ]
        ,
          -- Matching frame
          H.div
          { className: intercalate " "
              [ "form-group"
              , (fv.hasError' "matchingFrame") ?
                  "form-group--error" $
                  mempty
              ]
          }
          [
            H.div
            { className: "form-group__label" }
            [
              H.label {} [ H.text "Matching frame" ]
            ]
          ,
            H.div
            { className: "form-group__field" }
            [
              B.formInput $
              { type: "number"
              } `merge` bindStateKey "matchingFrame"
            ,
              R2.if' (fv.hasError' "matchingFrame") $
                H.div
                { className: "form-group__error" }
                [
                  H.text "Please enter an `Int` value (eg. 3)"
                ]
            ]
          ]
        ]
      ,
        -- Clique
        B.fieldset
        { titleSlot: H.text "Clique" }
        [
          -- Clique type
          H.div
          { className: intercalate " "
              [ "form-group"
              ]
          }
          [
            H.div
            { className: "form-group__label" }
            [
              H.label {} [ H.text "Type" ]
            ]
          ,
            H.div
            { className: "form-group__field" }
            [
              H.div
              { className: "btn-group"
              , role: "group"
              }
              [
                B.button
                { callback: \_ -> setter stateBox "cliqueType" $ show FIS_
                , variant: OutlinedButtonVariant Secondary
                , className: state.cliqueType == show FIS_ ?
                    "active" $
                    ""
                }
                [
                  H.text "FIS"
                ]
              ,
                B.button
                { callback: \_ -> setter stateBox "cliqueType" $ show MaxClique_
                , variant: OutlinedButtonVariant Secondary
                , className: state.cliqueType == show MaxClique_ ?
                    "active" $
                    ""
                }
                [
                  H.text "MaxClique"
                ]
              ]
            ]
          ]
        ,
          -- TYPE::FIS_
          R2.if' (state.cliqueType == show FIS_) $ R.fragment
          [
            -- Support
            H.div
            { className: intercalate " "
                [ "form-group"
                , (fv.hasError' "support") ?
                    "form-group--error" $
                    mempty
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                H.label {} [ H.text "Support" ]
              ]
            ,
              H.div
              { className: "form-group__field" }
              [
                B.formInput $
                  bindStateKey "support"
              ,
                R2.if' (fv.hasError' "support") $
                  H.div
                  { className: "form-group__error" }
                  [
                    H.text "Please enter an `Int` value (eg. 3)"
                  ]
              ]
            ]
          ,
            -- Size
            H.div
            { className: intercalate " "
                [ "form-group"
                , (fv.hasError' "size") ?
                    "form-group--error" $
                    mempty
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                H.label {} [ H.text "Size" ]
              ]
            ,
              H.div
              { className: "form-group__field" }
              [
                B.formInput $
                  bindStateKey "size"
              ,
                R2.if' (fv.hasError' "sjze") $
                  H.div
                  { className: "form-group__error" }
                  [
                    H.text "Please enter an `Int` value (eg. 3)"
                  ]
              ]
            ]
          ]
        ,
          -- TYPE::MaxClique_
          R2.if' (state.cliqueType == show MaxClique_) $ R.fragment
          [
            -- Size
            H.div
            { className: intercalate " "
                [ "form-group"
                , (fv.hasError' "size") ?
                    "form-group--error" $
                    mempty
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                H.label {} [ H.text "Size" ]
              ]
            ,
              H.div
              { className: "form-group__field" }
              [
                B.formInput $
                { type: "number"
                } `merge` bindStateKey "size"
              ,
                R2.if' (fv.hasError' "sjze") $
                  H.div
                  { className: "form-group__error" }
                  [
                    H.text "Please enter an `Int` value (eg. 3)"
                  ]
              ]
            ]
          ,
            -- Treshold
            H.div
            { className: intercalate " "
                [ "form-group"
                , (fv.hasError' "threshold") ?
                    "form-group--error" $
                    mempty
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                H.label {} [ H.text "Treshold" ]
              ]
            ,
              H.div
              { className: "form-group__field" }
              [
                B.formInput $
                { type: "number"
                } `merge` bindStateKey "threshold"
              ,
                R2.if' (fv.hasError' "threshold") $
                  H.div
                  { className: "form-group__error" }
                  [
                    H.text "Please enter a `Double` value (eg. 0.5)"
                  ]
              ]
            ]
          ,
            -- Clique filter
            H.div
            { className: intercalate " "
                [ "form-group"
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                H.label {} [ H.text "Filter type" ]
              ]
            ,
              H.div
              { className: "form-group__field" }
              [
                B.formSelect
                ( bindStateKey "cliqueFilter" )
                [
                  H.option
                  { value: show ByThreshold }
                  [ H.text "By threshold" ]
                ,
                  H.option
                  { value: show ByNeighbours }
                  [ H.text "By neighbours" ]
                ]
              ]
            ]
          ]
        ]
      ,
        -- Submit
        H.div { className: "document-form-creation__submit" }
        [
          B.button
          { callback: \_ -> onSubmit
          , status: props.status == Deferred ? Deferred $ Enabled
          , variant: ButtonVariant Primary
          , type: "submit"
          , block: true
          }
          [ H.text "Add" ]
        ]
      ]


type FormData =
  ( proximity     :: String
  , synchrony     :: String
  , quality       :: String
  , exportFilter  :: String
  -- TimeUnit
  , granularity   :: String
  , period        :: String
  , step          :: String
  , matchingFrame :: String
  -- Clique
  , cliqueType    :: String
  , support       :: String
  , size          :: String
  , threshold     :: String
  , cliqueFilter  :: String
  )

defaultData :: Record FormData
defaultData =
  { proximity     : ""
  , synchrony     : ""
  , quality       : ""
  , exportFilter  : ""
  , granularity   : ""
  , period        : ""
  , step          : ""
  , matchingFrame : ""
  , cliqueType    : show FIS_
  , support       : ""
  , size          : ""
  , threshold     : ""
  , cliqueFilter  : show ByThreshold
  }

-- formValidation :: Record FormData -> Effect VForm
-- formValidation r = foldl append mempty rules
--   where
--     rules =
--       [ FV.nonEmpty "title" r.title
--       , FV.nonEmpty "source" r.source
--       , FV.nonEmpty "authors" r.authors
--       , FV.date "date" r.date
--       ]
