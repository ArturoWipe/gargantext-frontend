module Gargantext.Components.Forest
  ( forest
  , forestLayout
  , forestLayoutWithTopBar
  , forestLayoutMain
  , forestLayoutRaw
  , Common
  , LayoutProps
  ) where

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Record.Extra as RX
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree (treeLoader)
import Gargantext.Components.TopBar (topBar)
import Gargantext.Ends (Frontends, Backend)
import Gargantext.Prelude
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, OpenNodes, unSessions)
import Gargantext.Types (Handed(..), reverseHanded, switchHanded)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest"

-- Shared by components here with Tree
type Common = 
  ( frontends    :: Frontends
  , handed       :: T.Cursor Handed
  , reloadRoot   :: T.Cursor T2.Reload
  , route        :: AppRoute
  , tasks        :: T.Cursor (Maybe GAT.Reductor)
  )

type LayoutProps =
  ( backend      :: T.Cursor Backend
  , reloadForest :: T.Cursor T2.Reload
  , sessions     :: T.Cursor Sessions
  , showLogin    :: T.Cursor Boolean
  | Common 
  )

type Props = (
    forestOpen :: T.Cursor OpenNodes
  | LayoutProps )
  
type TreeExtra = (
    forestOpen :: T.Cursor OpenNodes
  , session :: Session
  )

forest :: R2.Component Props
forest = R.createElement forestCpt

forestCpt :: R.Component Props
forestCpt = here.component "forest" cpt where
  cpt props@{ backend
            , forestOpen
            , frontends
            , handed
            , reloadForest
            , reloadRoot
            , route
            , sessions
            , showLogin
            , tasks } _ = do
    -- NOTE: this is a hack to reload the forest on demand
    tasks'        <- GAT.useTasks reloadRoot reloadForest
    R.useEffect' $ do
      _ <- T.write (Just tasks') tasks
      pure unit
    handed'       <- T.useLive T.unequal handed
    reloadForest' <- T.useLive T.unequal reloadForest
    reloadRoot'   <- T.useLive T.unequal reloadRoot
    forestOpen'   <- T.useLive T.unequal forestOpen
    sessions'     <- T.useLive T.unequal sessions
    -- TODO If `reloadForest` is set, `reload` state should be updated
    -- TODO fix tasks ref
    -- R.useEffect' $ do
      -- R.setRef tasks $ Just tasks'
      -- GUR.initializeI reloadForest reload
    R2.useCache
      ( frontends /\ route /\ sessions' /\ handed' /\ forestOpen'
        /\ reloadForest' /\ reloadRoot' /\ (fst tasks').storage )
      (cp handed' sessions' tasks')
        where
          common = RX.pick props :: Record Common
          cp handed' sessions' tasks' _ =
            pure $ H.div { className: "forest" }
              (A.cons (plus handed' showLogin backend) (trees handed' sessions' tasks'))
          trees handed' sessions' tasks' = (tree handed' tasks') <$> unSessions sessions'
          tree handed' tasks' s@(Session {treeId}) =
            treeLoader { forestOpen
                       , frontends
                       , handed: handed'
                       , reload: reloadForest
                       , reloadRoot
                       , root: treeId
                       , route
                       , session: s
                       , tasks } []

plus :: Handed -> T.Cursor Boolean -> T.Cursor Backend -> R.Element
plus handed showLogin backend = H.div { className: "row" }
  [ H.button { className: buttonClass
             , on: { click }
             , title }
      [ H.div { className: divClass } [ H.text " Log in/out " ] -- fa-lg
      , H.div {} [ H.text "    " ] ]
  ]
  --, H.div { "type": "", className: "fa fa-plus-circle fa-lg"} []
  --, H.div { "type": "", className: "fa fa-minus-circle fa-lg"} []
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
  where
    click _ = do
      -- _ <- T.modify (const Nothing) backend
      _ <- T.write true showLogin
      pure unit
    title = "Add or remove connections to the server(s)."
    divClass = "fa fa-universal-access"
    buttonClass =
      "btn btn-primary col-5 " <> switchHanded "ml-1 mr-auto" "mr-1 ml-auto" handed


forestLayout :: R2.Component LayoutProps
forestLayout props = R.createElement forestLayoutCpt props

forestLayoutCpt :: R.Component LayoutProps
forestLayoutCpt = here.component "forestLayout" cpt where
  cpt props@{ handed } children =
    pure $ R.fragment
      [ topBar { handed } [], forestLayoutMain props children ]

-- Renders its first child component in the top bar and the rest in
-- the main view.
forestLayoutWithTopBar :: R2.Component LayoutProps
forestLayoutWithTopBar props = R.createElement forestLayoutWithTopBarCpt props

forestLayoutWithTopBarCpt :: R.Component LayoutProps
forestLayoutWithTopBarCpt = here.component "forestLayoutWithTopBar" cpt where
  cpt props@{ handed } children = do
    let { head: topBarChild, tail: mainChildren } =
          fromMaybe { head: H.div {} [], tail: [] } $ A.uncons children
    pure $ R.fragment
      [ topBar { handed } [ topBarChild ]
      , forestLayoutMain props mainChildren ]

forestLayoutMain :: R2.Component LayoutProps
forestLayoutMain props = R.createElement forestLayoutMainCpt props

forestLayoutMainCpt :: R.Component LayoutProps
forestLayoutMainCpt = here.component "forestLayoutMain" cpt where
  cpt props children = pure $ forestLayoutRaw props [ mainPage {} children ]

forestLayoutRaw :: R2.Component LayoutProps
forestLayoutRaw props = R.createElement forestLayoutRawCpt props

forestLayoutRawCpt :: R.Component LayoutProps
forestLayoutRawCpt = here.component "forestLayoutRaw" cpt where
  cpt p@{ backend
        , frontends
        , reloadForest
        , reloadRoot
        , route
        , sessions
        , showLogin
        , tasks } children = do
    handed' <- T.useLive T.unequal p.handed
    forestOpen <- T2.useCursed $ Set.empty

    pure $ R2.row $ reverseHanded
      [ H.div { className: "col-md-2"
              , style: { paddingTop: "60px" } }
        (A.cons (forest' p.handed forestOpen) children)
      ] handed'
      where
        forest' handed forestOpen =
          forest { backend
                 , frontends
                 , forestOpen
                 , handed
                 , reloadForest
                 , reloadRoot
                 , route
                 , sessions
                 , showLogin
                 , tasks } []

mainPage :: R2.Component ()
mainPage = R.createElement mainPageCpt

-- mainPageCpt :: R.Memo ()
-- mainPageCpt = R.memo (here.component "mainPage" cpt) where
mainPageCpt :: R.Component()
mainPageCpt = here.component "mainPage" cpt
  where
    cpt _ children = do
      pure $ H.div { className: "col-md-10" }
        [ H.div { id: "page-wrapper" }
          [ H.div { className: "container-fluid" } children ]
        ]
