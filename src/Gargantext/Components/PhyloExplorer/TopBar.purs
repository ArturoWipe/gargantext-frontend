module Gargantext.Components.PhyloExplorer.TopBar
  ( topBar
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.PhyloExplorer.Types (Term(..), Source(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

-- @WIP: * change "source" default value "" to `Maybe String`

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.TopBar"

type Props =
  ( sourceCallback             :: String -> Effect Unit
  , sourceList                 :: Array Source
  , autocompleteSearchCallback :: String -> Effect (Maybe Term)
  , autocompleteSubmitCallback :: Maybe Term -> Effect Unit
  , key :: String
  )

topBar :: R2.Leaf Props
topBar = R2.leaf topBarCpt
topBarCpt :: R.Component Props
topBarCpt = here.component "main" cpt where
  cpt props _ = do
    -- States
    let defaultSource = ""
    let defaultSearch = ""
    let defaultResult = (Nothing :: Maybe Term)

    source /\ sourceBox <- R2.useBox' defaultSource
    search /\ searchBox <- R2.useBox' defaultSearch
    result /\ resultBox <- R2.useBox' defaultResult

    -- Behaviors
    onSourceChange <- pure $
          flip T.write sourceBox
      >=> props.sourceCallback

    onAutocompleteChange <- pure $
          flip T.write searchBox
      >=> props.autocompleteSearchCallback
      >=> flip T.write_ resultBox

    onAutocompleteSubmit <- pure $
      const $ props.autocompleteSubmitCallback result

    -- Render
    pure $

      H.div
      { className: "phylo-topbar" }
      [
        -- Source
        H.div
        { className: "phylo-topbar__source"}
        [
          B.formSelect
          { value: source
          , callback: onSourceChange
          } $
          [
            H.option
            { disabled: true
            , value: ""
            }
            [ H.text "Select a source" ]
          ,
            H.option
            { value: "unselect" }
            [ H.text "unselect ✕" ]
          ]
          <>
            flip map props.sourceList

              \(Source { id, label }) ->
                H.option
                { value: id }
                [ H.text label ]

        ]
      ,
        -- Search
        H.form
        { className: "phylo-topbar__autocomplete"
        }
        [
          B.formInput
          { className: "phylo-topbar__suggestion"
          , status: Idled
          , value: case result of
              Nothing                     -> ""
              Just (Term { label }) -> label
          -- (?) noop: see `onAutocompleteChange`
          , callback: const $ pure unit
          }
        ,
          B.formInput
          { className: "phylo-topbar__search"
          , callback: onAutocompleteChange
          , value: search
          , placeholder: "Find a term"
          }
        ,
          B.button
          { callback: onAutocompleteSubmit
          , type: "submit"
          , className: "phylo-topbar__submit"
          }
          [ H.text "" ]
        ]
      ]
