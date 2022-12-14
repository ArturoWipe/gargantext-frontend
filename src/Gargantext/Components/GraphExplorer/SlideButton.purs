module Gargantext.Components.GraphExplorer.SlideButton
  ( Props
  , sizeButton
  , labelSizeButton
  , mouseSelectorSizeButton
  ) where

import Global (readFloat)
import Prelude
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.SlideButton"

type Props =
  ( caption  :: String
  , min      :: Number
  , max      :: Number
  , onChange :: forall e. e -> Effect Unit
  , state    :: T.Box Number
  )

sizeButton :: Record Props -> R.Element
sizeButton props = R.createElement sizeButtonCpt props []

sizeButtonCpt :: R.Component Props
sizeButtonCpt = here.component "sizeButton" cpt where
  cpt { state, caption, min, max, onChange } _ = do
    defaultValue <- T.useLive T.unequal state
    pure $ H.span { className: "range-simple" }
      [ H.label {} [ R2.small {} [ H.text caption ] ]
      , H.input { type: "range"
                , className: "form-control"
                , min: show min
                , max: show max
                , defaultValue
                , on: { input: onChange } }]

labelSizeButton :: R.Ref Sigmax.Sigma -> T.Box Number -> R.Element
labelSizeButton sigmaRef state =
  sizeButton {
      state
    , caption: "Label Size"
    , min: 1.0
    , max: 30.0
    , onChange: \e -> do
      let sigma = R.readRef sigmaRef
      let newValue = readFloat $ R.unsafeEventValue e
      Sigmax.dependOnSigma sigma "[labelSizeButton] sigma: Nothing" $ \s -> do
        Sigma.setSettings s {
          defaultLabelSize: newValue
        , drawLabels: true
        , maxNodeSize: newValue / 2.5
        --, labelSizeRatio: newValue / 2.5
        }
      T.write_ newValue state
    }

mouseSelectorSizeButton :: R.Ref Sigmax.Sigma -> T.Box Number -> R.Element
mouseSelectorSizeButton sigmaRef state =
  sizeButton {
      state
    , caption: "Selector Size"
    , min: 1.0
    , max: 50.0
    , onChange: \e -> do
      let sigma = R.readRef sigmaRef
      let newValue = readFloat $ R.unsafeEventValue e
      Sigmax.dependOnSigma sigma "[mouseSelectorSizeButton] sigma: Nothing" $ \s -> do
        Sigma.setSettings s {
          mouseSelectorSize: newValue
        }
      T.write_ newValue state
  }
