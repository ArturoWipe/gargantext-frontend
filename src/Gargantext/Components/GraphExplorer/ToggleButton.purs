module Gargantext.Components.GraphExplorer.ToggleButton
  ( Props, toggleButton, toggleButtonCpt
  , controlsToggleButton
  , edgesToggleButton
  , sidebarToggleButton
  , treeToggleButton
  ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

type Props = ( state :: R.State Boolean, onMessage :: String, offMessage :: String )

toggleButton :: Record Props -> R.Element
toggleButton props = R.createElement toggleButtonCpt props []

toggleButtonCpt :: R.Component Props
toggleButtonCpt = R.hooksComponent "ToggleButton" cpt
  where
    cpt {state, onMessage, offMessage} _ = do
      let (toggled /\ setToggled) = state
      pure $
        H.span {}
          [
            H.button
              { className: "btn btn-primary", on: {click: \_ -> setToggled not } }
              [ H.text (text onMessage offMessage toggled) ]
          ]
    text on _off true = on
    text _on off false = off

controlsToggleButton :: R.State Boolean -> R.Element
controlsToggleButton state =
  toggleButton { state: state, onMessage: "Hide Controls", offMessage: "Show Controls" }

edgesToggleButton :: R.State Boolean -> R.Element
edgesToggleButton state =
  toggleButton { state: state, onMessage: "Hide Edges", offMessage: "Show Edges" }

treeToggleButton :: R.State Boolean -> R.Element
treeToggleButton state =
  toggleButton { state: state, onMessage: "Hide Tree", offMessage: "Show Tree" }

sidebarToggleButton :: R.State Boolean -> R.Element
sidebarToggleButton state =
  toggleButton { state: state, onMessage: "Hide Sidebar", offMessage: "Show Sidebar" }