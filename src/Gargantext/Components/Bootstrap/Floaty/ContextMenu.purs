module Gargantext.Components.Bootstrap.ContextMenu
  ( contextMenu
  , contextMenuItem
  ) where

import Gargantext.Prelude

import DOM.Simple as DOM
import DOM.Simple.Event as DE
import DOM.Simple.Types (DOMRect)
import DOM.Simple.Window (window)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, null)
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect (Effect)
import FFI.Simple ((..))
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Hooks.Scrollbar (useScrollbar)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix as SE
import Reactix.DOM.HTML as H
import Reactix.SyntheticEvent as RE

type Props =
  ( closeCallback :: Unit -> Effect Unit
  , x             :: Number
  , y             :: Number
  )

contextMenu :: R2.Component Props
contextMenu = R2.component component

componentName :: String
componentName = "b-context-menu"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props@{ closeCallback
            , x
            , y
            } children
      = R.unsafeHooksEffect (UUID.genUUID >>= pure <<< UUID.toString)
    >>= \uuid -> do
    -- | States
    -- |
    rootRef <- R.useRef (null :: Nullable DOM.Element)

    -- NOTE: Just some dummy width/height here, it should be set properly in the effect function later
    rect' /\ _ <- R2.useBox' $ Just $ R2.domRectFromRect
      { x
      , y
      , width: 100.0
      , height: 100.0
      }

    -- | Hooks
    -- |
    { enableScroll, disableScroll } <- useScrollbar

    R.useLayoutEffect1 [] do
      -- Mount
      disableScroll
      -- Unmount
      pure enableScroll

    -- | Computed
    -- |
    let
      containerId :: String
      containerId = componentName <> "-" <> uuid

      containerCallback :: forall e. SE.SyntheticEvent e -> Effect Unit
      containerCallback e =
        let
          eventTargetId :: Maybe String
          eventTargetId = SE.unsafeEventTarget e # flip DOM.attr "id"

          hasClickedOnContainer :: Boolean
          hasClickedOnContainer = maybe false (eq containerId) eventTargetId

        in
          when hasClickedOnContainer $ closeCallback unit

    -- | Render
    -- |
    R.createPortal
      [
        H.div
        { className: componentName
        , on: { click: containerCallback }
        , key: uuid
        , id: containerId
        , ref: rootRef
        }
        [
          case rect' of
            Nothing ->
              H.div
              { key: componentName
              , className: componentName <> "__inner"
              , data: { placement: "right", toggle: "popover" }
              }
              children

            Just r ->
              H.div
              { key: componentName
              , className: componentName <> "__inner"
              , style: position props r
              , data: { placement: "right", toggle: "popover" }
              }
              children
        ]
      ]
      <$> R2.getPortalHost


position :: Record Props -> DOMRect -> { left :: Number, top :: Number }
position mouse {width: menuWidth, height: menuHeight} = {left, top}
  where
    left = if isRight then mouse.x else mouse.x - menuWidth
    top = if isAbove then mouse.y else mouse.y - menuHeight
    isRight = screenWidth - mouse.x > menuWidth -- is there enough space to show above
    isAbove = screenHeight - mouse.y > menuHeight -- is there enough space to show to the right?
    screenWidth = window .. "innerWidth"
    screenHeight = window .. "innerHeight"

--------------------------------------------------------------

type ItemProps =
  ( callback    :: Unit -> Effect Unit
  | ItemOptions
  )

type ItemOptions =
  ( className   :: String
  , status      :: ComponentStatus
  )

itemOptions :: Record ItemOptions
itemOptions =
  { className   : ""
  , status      : Enabled
  }

contextMenuItem :: forall r. R2.OptComponent ItemOptions ItemProps r
contextMenuItem = R2.optComponent itemCpt itemOptions

itemComponentName :: String
itemComponentName = "b-context-menu-item"

itemCpt :: R.Component ItemProps
itemCpt = R.hooksComponent itemComponentName cpt where
  cpt props@{ callback
            , status
            } children = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , itemComponentName
        , itemComponentName <> "--" <> show status
        ]

      click = onClick status callback

    -- Render
    pure $

      H.div
      { className
      , on: { click }
      }
      children

-- | Clicked event will effectively be triggered according to the
-- | component status props
onClick ::
     ComponentStatus
  -> (Unit -> Effect Unit)
  -> RE.SyntheticEvent DE.Event
  -> Effect Unit
onClick status callback event = do
  RE.preventDefault event
  when (status == Enabled) $ callback unit
