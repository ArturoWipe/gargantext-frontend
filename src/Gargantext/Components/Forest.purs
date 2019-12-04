module Gargantext.Components.Forest where

import Prelude (const, ($), (<$>))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Ends (Frontends)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session(..), Sessions, unSessions)
import Gargantext.Components.Forest.Tree (treeView)
import Gargantext.Components.Forest.Memories as Memories
import Gargantext.Components.Forest.Memories (Memories)
import Gargantext.Utils.Reactix as R2

type Props =
  ( sessions  :: Sessions
  , route     :: AppRoute
  , frontends :: Frontends
  , showLogin :: R2.Setter Boolean
  , memories  :: R2.Reductor Memories Memories.Action
  )

forest :: Record Props -> R.Element
forest props = R.createElement forestCpt props []

forestCpt :: R.Component Props
forestCpt = R.staticComponent "G.C.Forest.forest" cpt where
  cpt {sessions, route, frontends, showLogin, memories} _ =
    R.fragment $ A.cons (plus showLogin) trees
    where
      trees = tree <$> unSessions sessions
      tree s@(Session {treeId}) =
        treeView { root: treeId, frontends, mCurrentRoute: Just route, session: s }

plus :: R2.Setter Boolean -> R.Element
plus showLogin =
  H.button {on: {click}, className: "btn btn-primary"}
  [ H.div { "type": ""
          , className: "fa fa-plus-circle fa-lg"
          } [H.text "Login"] ]
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]
  where
    click _ = do
      showLogin (const true)
