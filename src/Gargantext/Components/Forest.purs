module Gargantext.Components.Forest
  ( forest
  , forestLayout
  , Common
  , Props
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree (treeLoader)
import Gargantext.Ends (Frontends, Backend)
import Gargantext.Routes (AppRoute(..), appPath)
import Gargantext.Sessions (Session(..), Sessions, OpenNodes, unSessions)
import Gargantext.Types (Handed)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest"

-- Shared by components here with Tree
type Common =
  ( frontends      :: Frontends
  , handed         :: T.Box Handed
  , reloadMainPage :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  , route          :: T.Box AppRoute
  )

type Props =
  ( backend            :: T.Box (Maybe Backend)
  , forestOpen         :: T.Box OpenNodes
  , reloadForest       :: T2.ReloadS
  , sessions           :: T.Box Sessions
  , showLogin          :: T.Box Boolean
  , tasks              :: T.Box GAT.Storage
  | Common
  )

type TreeExtra = (
    forestOpen :: T.Box OpenNodes
  )

forest :: R2.Component Props
forest = R.createElement forestCpt
forestCpt :: R.Component Props
forestCpt = here.component "forest" cpt where
  cpt { backend
      , forestOpen
      , frontends
      , handed
      , reloadForest
      , reloadMainPage
      , reloadRoot
      , route
      , sessions
      , showLogin
      , tasks } _ = do
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
    pure $ H.div { className: "forest-layout-content" }
      (A.cons (plus { backend, handed, showLogin }) (trees handed' sessions'))
    where
      trees handed' sessions' = (tree handed') <$> unSessions sessions'
      tree handed' s@(Session {treeId}) =
        treeLoader { forestOpen
                   , frontends
                   , handed: handed'
                   , reload: reloadForest
                   , reloadMainPage
                   , reloadRoot
                   , root: treeId
                   , route
                   , session: s
                   , tasks } []

type Plus =
  ( backend   :: T.Box (Maybe Backend)
  , handed    :: T.Box Handed
  , showLogin :: T.Box Boolean )

plus :: R2.Leaf Plus
plus p = R.createElement plusCpt p []
plusCpt :: R.Component Plus
plusCpt = here.component "plus" cpt where
  cpt { backend, showLogin } _ = pure $

    H.div
    { className: "forest-layout-action" }
    [
      H.a
      { className: intercalate " "
          [ "btn btn-primary"
          , "forest-layout-action__button"
          ]
      , href: appPath Home
      }
      [
        H.i
        { className: "fa fa-home"
        , title: "Back to home"
        }
        []
      ]
    ,
      H.button
      { className: intercalate " "
          [ "btn btn-primary d-block"
          , "forest-layout-action__button"
          ]
      , on: { click }
      , title: "Add or remove connections to the server(s)."
      }
      [
        H.span
        { className: "fa fa-universal-access" }
        [ H.text " Log in/out " ]
      ]
    ]

  --, H.div { "type": "", className: "fa fa-plus-circle fa-lg"} []
  --, H.div { "type": "", className: "fa fa-minus-circle fa-lg"} []
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
    where
      click _ = do
        -- NOTE Reset backend in case G.C.N.Home.homeLayout set that to (Just b)
        -- from current url
        _ <- T.write Nothing backend
        T.write_ true showLogin


forestLayout :: R2.Component Props
forestLayout = R.createElement forestLayoutCpt
forestLayoutCpt :: R.Component Props
forestLayoutCpt = here.component "forestLayout" cpt where
  cpt p _ = pure $

    H.div { className: "forest-layout-wrapper col-md-2" }
    [
      H.div { className: "forest-layout" }
      [
        H.div { className: "forest-layout-top-teaser" } []
      ,
        forest p []
      ,
        H.div { className: "forest-layout-bottom-teaser" } []
      ]
    ]
