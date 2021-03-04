module Gargantext.Components.GraphExplorer.ToggleButton
  ( Props
  , toggleButton
  , toggleButtonCpt
  , controlsToggleButton
  , edgesToggleButton
  , louvainToggleButton
  , multiSelectEnabledButton
  , sidebarToggleButton
  , pauseForceAtlasButton
  , treeToggleButton
  ) where

import Prelude

import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here = R2.here "Gargantext.Components.GraphExplorer.ToggleButton"

type Props = (
    state :: R.State Boolean
  , onMessage :: String
  , offMessage :: String
  , style      :: String
  , onClick :: forall e. e -> Effect Unit
  )

toggleButton :: Record Props -> R.Element
toggleButton props = R.createElement toggleButtonCpt props []

toggleButtonCpt :: R.Component Props
toggleButtonCpt = here.component "toggleButton" cpt
  where
    cpt {state, onMessage, offMessage, onClick, style} _ = do
      let (toggled /\ _) = state

      pure $ H.button { className: "btn btn-outline-" <> style <> " " <> cls toggled
                      , on: {click: onClick}
                      } [ R2.small {} [ H.text (text onMessage offMessage toggled) ] ]

    cls true = "active"
    cls false = ""
    text on _off true = on
    text _on off false = off

controlsToggleButton :: R.State Boolean -> R.Element
controlsToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Controls"
    , offMessage: "Show Controls"
    , onClick: \_ -> snd state not
    , style: "light"
    }

type EdgesButtonProps = (
  state :: R.State SigmaxTypes.ShowEdgesState
)

edgesToggleButton :: Record EdgesButtonProps -> R.Element
edgesToggleButton props = R.createElement edgesToggleButtonCpt props []

edgesToggleButtonCpt :: R.Component EdgesButtonProps
edgesToggleButtonCpt = here.component "edgesToggleButton" cpt
  where
    cpt {state: (state /\ setState)} _ = do
      pure $ H.button { className: "btn btn-outline-primary " <> cls state
                      , on: { click: onClick setState }
                      } [ R2.small {} [ H.text (text state) ] ]

    text s = if SigmaxTypes.edgeStateHidden s then "Show edges" else "Hide edges"

    cls SigmaxTypes.EShow = ""
    cls _ = "active"

    -- TODO: Move this to Graph.purs to the R.useEffect handler which renders nodes/edges
    onClick setState _ = setState SigmaxTypes.toggleShowEdgesState

louvainToggleButton :: R.State Boolean -> R.Element
louvainToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Louvain off"
    , offMessage: "Louvain on"
    , onClick: \_ -> snd state not
    , style: "primary"
    }

multiSelectEnabledButton :: R.State Boolean -> R.Element
multiSelectEnabledButton state =
  toggleButton {
      state: state
    , onMessage: "Single-node"
    , offMessage: "Multi-node"
    , onClick: \_ -> snd state not
    , style : "primary"
    }

type ForceAtlasProps = (
  state :: R.State SigmaxTypes.ForceAtlasState
)

pauseForceAtlasButton :: Record ForceAtlasProps -> R.Element
pauseForceAtlasButton props = R.createElement pauseForceAtlasButtonCpt props []

pauseForceAtlasButtonCpt :: R.Component ForceAtlasProps
pauseForceAtlasButtonCpt = here.component "forceAtlasToggleButton" cpt
  where
    cpt {state: (state /\ setState)} _ = do
      pure $ H.button { className: "btn btn-outline-primary " <> cls state
                      , on: { click: onClick setState }
                      } [ R2.small {} [ H.text (text state) ] ]

    cls SigmaxTypes.InitialRunning = "active"
    cls SigmaxTypes.Running = "active"
    cls _ = ""

    text SigmaxTypes.InitialRunning = "Pause Force Atlas"
    text SigmaxTypes.InitialStopped = "Start Force Atlas"
    text SigmaxTypes.Running = "Pause Force Atlas"
    text SigmaxTypes.Paused = "Start Force Atlas"

    onClick setState _ = setState SigmaxTypes.toggleForceAtlasState

treeToggleButton :: R.State Boolean -> R.Element
treeToggleButton state =
  toggleButton {
      state: state
    , onMessage: "Hide Tree"
    , offMessage: "Show Tree"
    , onClick: \_ -> snd state not
    , style: "light"
    }

sidebarToggleButton :: R.State GET.SidePanelState -> R.Element
sidebarToggleButton (state /\ setState) = R.createElement el {} []
  where
    el = here.component "sidebarToggleButton" cpt
    cpt {} _ = do
      pure $ H.button { className: "btn btn-outline-light " <> cls state
                      , on: { click: onClick}
                      } [ R2.small {} [ H.text (text onMessage offMessage state) ] ]

    cls (GET.Opened _) = "active"
    cls _ = ""

    onMessage = "Hide Sidebar"
    offMessage = "Show Sidebar"
    text on _off (GET.Opened _)    = on
    text _on off GET.InitialClosed = off
    text _on off GET.Closed        = off

    onClick = \_ -> do
      setState $ \s -> case s of
        GET.InitialClosed -> GET.Opened GET.SideTabLegend
        GET.Closed        -> GET.Opened GET.SideTabLegend
        (GET.Opened _)    -> GET.Closed
