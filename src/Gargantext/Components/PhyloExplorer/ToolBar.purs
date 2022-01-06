module Gargantext.Components.PhyloExplorer.ToolBar
  ( toolBar
  ) where

import Gargantext.Prelude

import Data.Array (intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.PhyloExplorer.Types (DisplayView(..))
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( resetViewCallback   :: Unit -> Effect Unit
  , displayView         :: DisplayView
  , changeViewCallback  :: DisplayView -> Effect Unit
  , exportCallback      :: Unit -> Effect Unit
  , isolineBox          :: T.Box Boolean
  )

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.ToolBar"

toolBar :: R2.Leaf Props
toolBar = R2.leaf component

component :: R.Component Props
component = here.component "main" cpt where
  cpt { resetViewCallback
      , displayView
      , changeViewCallback
      , exportCallback
      , isolineBox
      } _ = do
    -- States
    isIsolineDisplayed <- R2.useLive' isolineBox

    -- Render
    pure $

      H.div
      { className: "phylo-toolbar" }
      [
        -- View settings
        B.fieldset
        { className: "phylo-toolbar__section"
        , titleSlot: H.text "View settings"
        }
        [
          -- Reset view button
          B.button
          { callback: resetViewCallback
          , variant: OutlinedButtonVariant Secondary
          }
          [
            H.text "Reset view"
          ]
        ,
          H.span
          { className: "phylo-toolbar__gap" }
          [ H.text "" ]
        ,
          -- Display view mode
          H.div
          { className: "btn-group"
          , role: "group"
          }
          [
            B.button
            { title: "Show node header"
            , callback: \_ -> changeViewCallback HeadingMode
            , variant: OutlinedButtonVariant Secondary
            , className: displayView == HeadingMode ?
                "active" $
                ""
            }
            [
              B.icon
              { name: "header" }
            ]
          ,
            B.button
            { title: "Show node inner label"
            , callback: \_ -> changeViewCallback LabelMode
            , variant: OutlinedButtonVariant Secondary
            , className: displayView == LabelMode ?
                "active" $
                ""
            }
            [
              B.icon
              { name: "dot-circle-o" }
            ]
          ,
            B.button
            { title: "Show default landing view"
            , callback: \_ -> changeViewCallback LandingMode
            , variant: OutlinedButtonVariant Secondary
            , className: displayView == LandingMode ?
                "active" $
                ""
            }
            [
              B.icon
              { name: "circle" }
            ]
          ]
        ]
      ,
        -- View settings
        B.fieldset
        { className: "phylo-toolbar__section"
        , titleSlot: H.text "View settings"
        }
        [
          -- Isoline button
          B.button
          { callback: \_ -> T.modify_ not isolineBox
          , variant: isIsolineDisplayed ?
              ButtonVariant Secondary $
              OutlinedButtonVariant Secondary
          }
          [
            H.text "Iso Line data"
          ]
        ,
          H.span
          { className: "phylo-toolbar__gap" }
          [ H.text "" ]
        ,
          -- Screenshot button
          B.button
          { callback: exportCallback
          , variant: OutlinedButtonVariant Secondary
          }
          [
            H.text "Take screenshot"
          ]
        ]
      ]
