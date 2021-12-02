module Gargantext.Components.PhyloExplorer.TopBar
  ( topBar
  ) where

import Gargantext.Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.Types (Source(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

-- @WIP: * change "source" default value "" to `Maybe String`

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.TopBar"

type Props =
  ( sourceCallback  :: String -> Effect Unit
  , sourceList      :: Array Source
  )

topBar :: R2.Leaf Props
topBar = R2.leaf topBarCpt
-- topBarCpt :: R.Component Props
-- topBarCpt = here.component "main" cpt where
--   cpt props _ = do
--     -- States
--     let defaultSource = ""

--     source /\ sourceBox <- R2.useBox' defaultSource

--     -- Effects
--     R.useEffect1' source $ props.sourceCallback source

--     -- Render
--     pure $

--       H.div
--       { className: "phylo-topbar" }
--       [
--         B.formSelect
--         { className: "select-source"
--         , value: source
--         , callback: flip T.write_ sourceBox
--         } $
--         [
--           H.option
--           { disabled: true
--           , value: ""
--           }
--           [ H.text "select a source ↴" ]
--         ,
--           H.option
--           { value: "unselect" }
--           [ H.text "unselect source ✕" ]
--         ]
--         <>
--           flip map props.sourceList
--           ( \(Source { id, label }) ->
--               H.option
--               { value: id }
--               [ H.text label ]
--           )
--       ]

topBarCpt :: R.Component Props
topBarCpt = here.component "main" cpt where
  cpt props _ = do
    -- States
    let defaultSource = ""

    source /\ sourceBox <- R2.useBox' defaultSource

    -- @onChange
    onChange <- pure $ \new -> do
      T.write_ new sourceBox
      props.sourceCallback new

    -- Render
    pure $

      H.div
      { className: "phylo-topbar" }
      [
        B.formSelect
        { className: "select-source"
        , value: source
        , callback: onChange
        } $
        [
          H.option
          { disabled: true
          , value: ""
          }
          [ H.text "select a source ↴" ]
        ,
          H.option
          { value: "unselect" }
          [ H.text "unselect source ✕" ]
        ]
        <>
          flip map props.sourceList
          ( \(Source { id, label }) ->
              H.option
              { value: id }
              [ H.text label ]
          )
      ]
