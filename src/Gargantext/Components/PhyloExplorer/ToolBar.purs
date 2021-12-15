module Gargantext.Components.PhyloExplorer.ToolBar
  ( toolBar
  ) where

import Gargantext.Prelude

import Data.Array (intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( nodeId              :: NodeID
  , resetViewCallback   :: Unit -> Effect Unit
  )

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.ToolBar"

toolBar :: R2.Leaf Props
toolBar = R2.leaf component

component :: R.Component Props
component = here.component "main" cpt where
  cpt props _ = do
    -- States
    let dropdownId = "phylo-toolbar-dropdown--" <> show props.nodeId

    -- Render
    pure $

      H.div
      { className: "phylo-toolbar" }
      [
        -- Reset view button
        B.button
        { className: "phylo-toolbar__reset"
        , callback: props.resetViewCallback
        , variant: OutlinedButtonVariant Secondary
        }
        [
          H.text "Reset view"
        ]
      ,
        -- Node display mode

        -- (?) Bootstrap "dropdown" is best for declarative integration
        -- (see https://gitlab.iscpif.fr/pdominique/hello-gargan/wikis/4%E2%80%94UI-library#other-re-usable-logic )
        H.div
        { className: intercalate " "
            [ "phylo-toolbar__node"
            , "dropdown"
            ]
        }
        [
          H.div
          { className: "btn btn-secondary dropdown-toggle"
          , type: "button"
          , id: dropdownId
          }
          [
            H.text "Node display mode"
          ]
        ,
          H.div
          { className: "dropdown-menu"
          , "aria-labelledby": dropdownId
          }
          [
            H.a
            { className: "dropdown-item" }
            [
              H.text "Label"
            ]
          ,
            H.a
            { className: "dropdown-item" }
            [
              H.text "Heading"
            ]
          ,
            H.a
            { className: "dropdown-item" }
            [
              H.text "Clear"
            ]
          ]
        ]
      ,
        -- Screenshot button
        B.button
        { className: "phylo-toolbar__screenshot"
        , callback: \_ -> pure unit
        , variant: OutlinedButtonVariant Secondary
        }
        [
          H.text "Take screenshot"
        ]
      ]
