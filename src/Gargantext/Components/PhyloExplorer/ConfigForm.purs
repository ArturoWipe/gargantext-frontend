module Gargantext.Components.PhyloExplorer.ConfigForm
  ( configForm
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log, log3)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.PhyloExplorer.API (Clique(..), CliqueFilter(..), ReflexiveClique(..), ReflexiveTimeUnit(..), TimeUnit(..), TimeUnitCriteria(..), UpdateData(..), extractCriteria, fromReflexiveTimeUnit, toReflexiveTimeUnit)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Hooks.FormValidation (VForm, useFormValidation)
import Gargantext.Hooks.FormValidation.Unboxed as FV
import Gargantext.Hooks.StateRecord (useStateRecord)
import Gargantext.Hooks.StateRecord.Behaviors (setter)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post, get)
import Gargantext.Types as GT
import Gargantext.Utils (getter, nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record (merge)
import Record as Record
import Record.Extra (pick)
import Toestand as T
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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
    , setStateKey
    } <- useStateRecord $ parseFormData (pick props :: Record FormData)
    fv <- useFormValidation

    R.useEffect1' state $ log state

  -- Behaviors
    let

      -- @onSubmit: exec whole form validation and execute callback
      onSubmit = do

        -- result <- fv.try (\_ -> formValidation state)
        result <- pure $ Right state
        case result of
          Left err -> log3 "configForm validation error" state err
          Right _  -> props.callback $ parseRawData state

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
                { callback: \_ -> setter stateBox "cliqueType" FIS_
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
                { callback: \_ -> setter stateBox "cliqueType" MaxClique_
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
          R2.if' (state.cliqueType == show FIS_) $R.fragment
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
          R2.if' (state.cliqueType == show MaxClique_) $R.fragment
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
  ( proximity     :: Number
  , synchrony     :: Number
  , quality       :: Number
  , exportFilter  :: Number
  -- TimeUnit
  , timeUnit      :: TimeUnit
  -- Clique


  -- , cliqueType    :: ReflexiveClique
  -- , support       :: Int
  -- , size          :: Int
  -- , threshold     :: Number
  -- , cliqueFilter  :: CliqueFilter
  , clique :: Clique
  )

defaultData :: Record FormData
defaultData =
  { proximity: 0.1
  , synchrony: 0.1
  , quality: 0.1
  , exportFilter: 0.1
  -- TimeUnit
  , timeUnit: Year $ TimeUnitCriteria
    { period: 3
    , step: 1
    , matchingFrame: 5
    }
  -- Clique
  -- , cliqueType: FIS_
  -- , support: 1
  -- , size: 1
  -- , threshold: 0.5
  -- , cliqueFilter: ByThreshold
  , clique: FIS
      { support: 1
      , size: 1
      }
  }

type RawData =
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

-- (?) due to `Clique` multi constructors nature, we have to relying on a
--     set of default data for every constructor property
defaultCliqueData ::
  { cliqueType    :: String
  , support       :: String
  , size          :: String
  , threshold     :: String
  , cliqueFilter  :: String
  }
defaultCliqueData =
  { cliqueType: "FIS_"
  , support: "1"
  , size: "1"
  , threshold: "0.5"
  , cliqueFilter: "ByThreshold"
  }

parseFormData :: Record FormData -> Record RawData
parseFormData { proximity
              , synchrony
              , quality
              , exportFilter
              , timeUnit
              , clique
              }
  = { proximity: show proximity
    , synchrony: show synchrony
    , quality: show quality
    , exportFilter: show exportFilter
    -- Time unit
    , granularity: timeUnit #
        (show <<< toReflexiveTimeUnit)
    , period: timeUnit #
        (show <<< getter _.period <<< extractCriteria)
    , step: timeUnit #
        (show <<< getter _.step <<< extractCriteria)
    , matchingFrame: timeUnit #
        (show <<< getter _.matchingFrame <<< extractCriteria)
    -- Clique
    } `merge` (parseClique clique)
    where
      parseClique :: Clique ->
        { cliqueType    :: String
        , support       :: String
        , size          :: String
        , threshold     :: String
        , cliqueFilter  :: String
        }
      parseClique (FIS o) =
        { support: show o.support
        , size: show o.size
        } `merge` defaultCliqueData
      parseClique (MaxClique o) =
        { size: show o.size
        , threshold: show o.threshold
        , cliqueFilter: show o.filter
        } `merge` defaultCliqueData


parseRawData :: Record RawData -> Record FormData
parseRawData raw@{ proximity
               , synchrony
               , quality
               , exportFilter
               , granularity
               , period
               , step
               , matchingFrame
               }
  = { proximity: fromMaybe 0.0 (Number.fromString proximity)
    , synchrony: fromMaybe 0.0 (Number.fromString synchrony)
    , quality: fromMaybe 0.0 (Number.fromString quality)
    , exportFilter: fromMaybe 0.0 (Number.fromString exportFilter)
    -- Time unit
    , timeUnit: parseTimeUnit
        (parseCriteria period step matchingFrame)
        granularity
    -- Clique
    , clique: parseClique raw raw.cliqueType
  }
  where
    parseCriteria :: String -> String -> String -> TimeUnitCriteria
    parseCriteria a b c = TimeUnitCriteria
      { period       : fromMaybe 0 (Int.fromString a)
      , step         : fromMaybe 0 (Int.fromString b)
      , matchingFrame: fromMaybe 0 (Int.fromString c)
      }

    parseTimeUnit :: TimeUnitCriteria -> String -> TimeUnit
    parseTimeUnit criteria = read >>> case _ of
      Nothing                       -> Year criteria
      Just (r :: ReflexiveTimeUnit) -> fromReflexiveTimeUnit r criteria

    parseClique :: Record RawData -> String -> Clique
    parseClique o = read >>> case _ of
      Nothing -> FIS
        { support: 1
        , size: 1
        }
      Just (r :: ReflexiveClique) -> case r of
        FIS_ -> FIS
          { support: fromMaybe 0 (Int.fromString o.support)
          , size: fromMaybe 0 (Int.fromString o.size)
          }
        MaxClique_ -> MaxClique
          { size: fromMaybe 0 (Int.fromString o.support)
          , threshold: fromMaybe 0.0 (Number.fromString o.threshold)
          , filter: fromMaybe ByThreshold (read o.cliqueFilter)
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
