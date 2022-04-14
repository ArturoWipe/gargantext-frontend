module Gargantext.Components.Forest
  ( forestLayout
  , Props
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Position(..), TooltipPosition(..), Variant(..))
import Gargantext.Components.Forest.Tree (treeLoader)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.LinkHandler (useLinkHandler)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Sessions (Session(..), unSessions)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

moduleName :: R2.Module
moduleName = "Gargantext.Components.Forest"

-- Shared by components here with Tree
type Props =
  ( boxes     :: Boxes
  , frontends :: Frontends
  )

forestLayout :: B.Leaf Props
forestLayout = B.leaf' (moduleName <> "forestLayout") cpt where
  cpt p _ = pure $

    H.div
    { className: "forest-layout" }
    [
      H.div { className: "forest-layout__top-teaser" } []
    ,
      forest p
    ,
      H.div { className: "forest-layout__bottom-teaser" } []
    ]

forest :: B.Leaf Props
forest = B.leaf (moduleName <> "forest") cpt where
  cpt { boxes: boxes@{ handed
                     , reloadForest
                     , sessions }
      , frontends } _ = do
    -- TODO Fix this. I think tasks shouldn't be a Box but only a Reductor
    -- tasks'        <- GAT.useTasks reloadRoot reloadForest
    -- R.useEffect' $ do
    --   T.write_ (Just tasks') tasks
    handed'       <- T.useLive T.unequal handed
    sessions'     <- T.useLive T.unequal sessions
    -- forestOpen'   <- T.useLive T.unequal forestOpen
    -- reloadRoot'   <- T.useLive T.unequal reloadRoot
    -- route'        <- T.useLive T.unequal route

    -- TODO If `reloadForest` is set, `reload` state should be updated
    -- TODO fix tasks ref
    pure $ R.fragment
      (A.cons (plus { boxes }) (trees handed' sessions'))
    where
      trees handed' sessions' = (tree handed') <$> unSessions sessions'
      tree handed' s@(Session { treeId }) =
        H.div
        { className: "forest-layout-tree" }
        [
          treeLoader
          { boxes
          , frontends
          , handed: handed'
          , reload: reloadForest
          , root: treeId
          , session: s
          }
        ]

--------------------------------------------------------

type Plus = ( boxes :: Boxes )

plus :: B.Leaf Plus
plus = B.leaf (moduleName <> "plus") cpt where
  cpt { boxes: { backend, showLogin } } _ = do
    -- Hooks
    { goToRoute } <- useLinkHandler

    -- Behaviors
    let
      click _ = do
        -- NOTE Reset backend in case G.C.N.Home.homeLayout set that to (Just b)
        -- from current url
        _ <- T.write Nothing backend
        T.write_ true showLogin

    -- Render
    pure $
      H.div
      { className: "forest-layout-action" }
      [
        -- H.a
        -- { className: intercalate " "
        --     [ "btn btn-primary"
        --     , "forest-layout-action__button"
        --     ]
        -- , href: appPath Home
        -- }
        -- [
        --   H.i
        --   { className: "fa fa-home"
        --   , title: "Back to home"
        --   }
        --   []
        -- ]
        B.tooltipContainer
        { delayShow: 600
        , position: TooltipPosition Top
        , tooltipSlot:
            B.span_ "Back to home"
        , defaultSlot:
            B.button
            { className: "forest-layout-action__button"
            , callback: const $ goToRoute Home
            , variant: ButtonVariant Light
            }
            [
              B.icon { name: "home" }
            ]
        }
      ,
        B.tooltipContainer
        { delayShow: 600
        , position: TooltipPosition Top
        , tooltipSlot:
            B.span_ "Add or remove connection to the server(s)"
        , defaultSlot:
            B.button
            { className: "forest-layout-action__button"
            , callback: click
            , variant: ButtonVariant Light
            }
            [
              B.icon
              { name: "universal-access" }
            ,
              B.wad_ [ "d-inline-block", "w-1" ]
            ,
              H.text $ "Log in/out"
            ]
        }
      ]
  --, H.div { "type": "", className: "fa fa-plus-circle fa-lg"} []
  --, H.div { "type": "", className: "fa fa-minus-circle fa-lg"} []
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
