module Gargantext.Components.PhyloExplorer.TopBar
  ( topBar
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.PhyloExplorer.Types (Term(..), Source(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix (nothing)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

-- @WIP: * change "source" default value "" to `Maybe String`
type Props =
  ( sources             :: Array (Source)
  , source              :: T.Box (String)
  , toolBar             :: T.Box (Boolean)
  , sideBar             :: T.Box (Boolean)
  , result              :: T.Box (Maybe Term)
  , search              :: T.Box (String)
  , submit              :: Unit -> Effect Unit
  )

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.TopBar"

topBar :: R2.Leaf Props
topBar = R2.leaf component

component :: R.Component Props
component = here.component "main" cpt where
  cpt { sources
      , source
      , toolBar
      , sideBar
      , search
      , result
      , submit } _ = do
    -- States
    source'   <- R2.useLive' source
    toolBar'  <- R2.useLive' toolBar
    sideBar'  <- R2.useLive' sideBar
    search'   <- R2.useLive' search
    result'   <- R2.useLive' result

    -- Render
    pure $

      H.div
      { className: "phylo-topbar" }
      [
        -- Toolbar toggle
        B.button
        { className: "phylo-topbar__toolbar"
        , callback: \_ -> T.modify_ (not) toolBar
        , variant: toolBar' ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [ H.text $ toolBar' ? "Hide toolbar" $ "Show toolbar" ]
      ,
        -- Sidebar toggle
        B.button
        { className: "phylo-topbar__sidebar"
        , callback: \_ -> T.modify_ (not) sideBar
        , variant: sideBar' ?
            ButtonVariant Light $
            OutlinedButtonVariant Light
        }
        [ H.text $ sideBar' ? "Hide sidebar" $ "Show sidebar" ]
      ,
        -- Source
        H.div
        { className: "phylo-topbar__source"}
        [
          B.formSelect
          { value: source'
          , callback: flip T.write_ source
          } $
          [
            H.option
            { value: ""
            }
            [ H.text "Select a source" ]
          ]
          <>
            flip map sources

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
          , value: case result' of
              Nothing                     -> ""
              Just (Term { label }) -> label
          -- (?) noop: see `submit`
          , callback: \_ -> nothing
          }
        ,
          B.formInput
          { className: "phylo-topbar__search"
          , callback: flip T.write_ search
          , value: search'
          , placeholder: "Find a term"
          }
        ,
          B.button
          { callback: submit
          , type: "submit"
          , className: "phylo-topbar__submit"
          }
          [ H.text "" ]
        ]
      ]
