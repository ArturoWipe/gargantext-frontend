-- | The RangeSlider is a slider component with two knobs, allowing
-- | the user to specify both a minimum and maximum value to filter
-- | data by. It may be dragged with the mouse or moved with the
-- | keyboard like a regular slider component.  The RangeSlider is
-- | designed to let the user adjust in multiples of a provided
-- | epsilon (smallest difference)
module Gargantext.Components.RangeSlider where

import Prelude
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null, toMaybe)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import DOM.Simple as DOM
import DOM.Simple.Console (log, log2)
import DOM.Simple.Document (document)
import DOM.Simple.Element as Element
import DOM.Simple.Event as Event
import DOM.Simple.EventListener as EL
import DOM.Simple.Types (DOMRect, Element)
import Global (toFixed)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
--import Global (toFixed)
import Math as M
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.SyntheticEvent as RE

import Gargantext.Utils.Math (roundToMultiple)
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
-- data Axis = X | Y

-- To avoid overloading the terms 'min' and 'max' here, we treat 'min'
-- and 'max' as being the bounds of the scale and 'low' and 'high' as
-- being the selected values
type Props =
  ( bounds :: Range.NumberRange       -- The minimum and maximum values it is possible to select
  , initialValue :: Range.NumberRange -- The user's selection of minimum and maximum values
  , epsilon :: Number           -- The smallest possible change (for mouse)
  , step :: Number              -- The 'standard' change (for keyboard)
  -- , axis :: Axis                -- Which direction to move in
  , width :: Number
  , height :: Number
  , onChange :: Range.NumberRange -> Effect Unit )

rangeSlider :: Record Props -> R.Element
rangeSlider props = R.createElement rangeSliderCpt props []

data Knob = MinKnob | MaxKnob

data RangeUpdate = SetMin Number | SetMax Number

rangeSliderCpt :: R.Component Props
rangeSliderCpt = R.hooksComponent "RangeSlider" cpt
  where
    cpt props _ = do
      --R.useEffect' $ do
      --  liftEffect $ log2 "Props: " props

      -- rounding precision (i.e. how many decimal digits are in epsilon)
      let precision = fromMaybe 0 $ fromNumber $ max 0.0 $ - M.floor $ (M.log props.epsilon) / M.ln10

      -- scale bar
      scaleElem <- (R.useRef null) :: R.Hooks (R.Ref (Nullable DOM.Element)) -- dom ref
      --scalePos <- R2.usePositionRef scaleElem
      -- low knob
      lowElem <- (R.useRef null) :: R.Hooks (R.Ref (Nullable DOM.Element)) -- a dom ref to the low knob
      --lowPos <- R2.usePositionRef lowElem
      -- high knob
      highElem <- (R.useRef null) :: R.Hooks (R.Ref (Nullable DOM.Element)) -- a dom ref to the high knob
      --highPos <- R2.usePositionRef highElem
      -- The value of the user's selection
      value /\ setValue <- R.useState' $ initialValue props
      let Range.Closed value' = value

      -- the knob we are currently in a drag for. set by mousedown on a knob
      dragKnob /\ setDragKnob <- R.useState' $ (Nothing :: Maybe Knob)
      -- the bounding box within which the mouse can drag
      --dragScale <- R.useRef $ Nothing

      -- the handler functions for trapping mouse events, so they can be removed
      mouseMoveHandler <- (R.useRef $ Nothing) :: R.Hooks (R.Ref (Maybe (EL.Callback Event.MouseEvent)))
      mouseUpHandler <- (R.useRef $ Nothing) :: R.Hooks (R.Ref (Maybe (EL.Callback Event.MouseEvent)))
      let destroy = \_ -> do
            --log "RangeSlider: Destroying event handlers"
            destroyEventHandler "mousemove" mouseMoveHandler
            destroyEventHandler "mouseup" mouseUpHandler
            R.setRef mouseMoveHandler $ Nothing
            R.setRef mouseUpHandler $ Nothing

      R2.useLayoutEffect1' dragKnob $ \_ -> do
        let scalePos' = R.readRef scaleElem
        let scalePos = Element.boundingRect <$> toMaybe scalePos'
        let lowPos' = R.readRef lowElem
        let lowPos = Element.boundingRect <$> toMaybe lowPos'
        let highPos' = R.readRef highElem
        let highPos = Element.boundingRect <$> toMaybe highPos'

        case dragKnob of
          Just knob -> do
            let drag = (getDragScale knob scalePos lowPos highPos) :: Maybe Range.NumberRange

            --R.setRef dragScale drag
            let onMouseMove = EL.callback $ \(event :: Event.MouseEvent) -> do
                  -- log2 "dragKnob" dragKnob
                  -- log2 "lowPos" lowPos
                  -- log2 "highPos" highPos
                  -- log2 "drag" drag
                  -- log2 "scale" scalePos
                  -- -- log2 "value" value
                  -- let (R2.Point mousePos) = R2.domMousePosition event
                  -- log2 "mouse position" mousePos
                  -- let scale = rectRange <$> scalePos
                  -- case scale of
                  --   Just scale_ ->
                  --     case drag of
                  --       Just drag_ -> do
                  --         let normal = Range.normalise scale_ (Range.clamp drag_ mousePos.x)
                  --         log2 "normal" normal
                  --         log2 "project normal" $ Range.projectNormal props.bounds normal
                  --       _ -> log "drag is Nothing"
                  --   _ ->  log "scale is Nothing"

                  case reproject drag scalePos props.bounds (R2.domMousePosition event) of
                    Just val -> do
                      --log2 "reproject val" val
                      setKnob knob setValue value $ round props.epsilon props.bounds val
                    Nothing -> destroy unit
            let onMouseUp = EL.callback $ \(_event :: Event.MouseEvent) -> do
                  props.onChange value
                  setDragKnob $ const Nothing
                  destroy unit
            --log "RangeSlider: Creating event handlers"
            --log2 "Clamp: " $ Range.clamp props.bounds value'.min
            EL.addEventListener document "mousemove" onMouseMove
            EL.addEventListener document "mouseup" onMouseUp
            R.setRef mouseMoveHandler $ Just onMouseMove
            R.setRef mouseUpHandler $ Just onMouseUp
          Nothing -> destroy unit
      pure $ H.div { className, aria }
        [ renderScale scaleElem props value'
        , renderKnob MinKnob lowElem  value'.min props.bounds setDragKnob precision
        , renderKnob MaxKnob highElem value'.max props.bounds setDragKnob precision
        ]
    className = "range-slider"
    aria = { label: "Range Slider Control. Expresses filtering data by a minimum and maximum value range through two slider knobs. Knobs can be adjusted with the arrow keys." }

destroyEventHandler
  :: forall e
  .  Event.IsEvent e
  => String -> R.Ref (Maybe (EL.Callback e)) -> Effect Unit
destroyEventHandler name ref = traverse_ destroy $ R.readRef ref
  where
    destroy handler = do
      EL.removeEventListener document name handler
      R.setRef ref Nothing

setKnob :: Knob -> R2.StateSetter Range.NumberRange -> Range.NumberRange -> Number -> Effect Unit
setKnob knob setValue r val = setValue $ const $ setter knob r val
  where
    setter MinKnob = Range.withMin
    setter MaxKnob = Range.withMax

getDragScale :: Knob -> Maybe DOMRect -> Maybe DOMRect -> Maybe DOMRect -> Maybe Range.NumberRange
getDragScale knob scalePos lowPos highPos = do
  scale <- scalePos
  low <- lowPos
  high <- highPos
  pure $ Range.Closed { min: min knob scale low, max: max knob scale high }
  where
    min MinKnob scale _ = scale.left
    min MaxKnob _ low = low.left
    max MinKnob _ high = high.left
    max MaxKnob scale _ = scale.right

renderScale ref {width,height} {min, max} =
   H.div { ref, className, width, height, aria, style } []
  where
    className = "scale"
    aria = { label: "Scale running from " <> show min <> " to " <> show max }
    style = { width: "100%", height: "3px" }

renderKnob :: Knob -> R.Ref (Nullable DOM.Element) -> Number -> Range.NumberRange -> R2.StateSetter (Maybe Knob) -> Int -> R.Element
renderKnob knob ref val bounds set precision =
  H.div { ref, tabIndex, className, aria, onMouseDown, style } [
      H.div { className: "button" } []
    , H.text $ text $ toFixed precision val
  ]
  where
    text (Just num) = num
    text Nothing = "error"
    tabIndex = 0
    className = "knob"
    aria = { label: labelPrefix knob <> "value: " <> show val }
    labelPrefix MinKnob = "Minimum "
    labelPrefix MaxKnob = "Maximum "
    onMouseDown = mkEffectFn1 $ \_ -> set $ const $ Just knob
    percOffset = Range.normalise bounds val
    style = { left: (show $ 100.0 * percOffset) <> "%" }

-- TODO round to nearest epsilon
reproject :: Maybe Range.NumberRange -> Maybe DOMRect -> Range.NumberRange -> R2.Point -> Maybe Number
reproject drag scalePos value (R2.Point mousePos) = do
  drag_ <- drag
  scale_ <- rectRange <$> scalePos
  let normal = Range.normalise scale_ (Range.clamp drag_ mousePos.x)
  pure $ Range.projectNormal value normal

rectRange :: DOMRect -> Range.NumberRange
rectRange rect = Range.Closed { min, max }
  where min = rect.left
        max = rect.right

initialValue :: Record Props -> Range.NumberRange
initialValue props = roundRange props.epsilon props.bounds props.initialValue

round :: Number -> Range.NumberRange -> Number -> Number
round epsilon bounds = roundToMultiple epsilon <<< Range.clamp bounds

roundRange :: Number -> Range.NumberRange -> Range.NumberRange -> Range.NumberRange
roundRange epsilon bounds (Range.Closed initial) = Range.Closed { min, max }
  where min = round epsilon bounds initial.min
        max = round epsilon bounds initial.max
  
