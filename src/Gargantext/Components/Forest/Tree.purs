module Gargantext.Components.Forest.Tree where

import DOM.Simple.Console (log, log2)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RecordE

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node (nodeSpan)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (AddNodeValue(..), addNode)
import Gargantext.Components.Forest.Tree.Node.Action.Delete (deleteNode, unpublishNode)
import Gargantext.Components.Forest.Tree.Node.Action.Move   (moveNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Merge  (mergeNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Link   (linkNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (RenameValue(..), rename)
import Gargantext.Components.Forest.Tree.Node.Action.Share   as Share
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Update (updateRequest)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFile, uploadArbitraryFile)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Ends (Frontends, toUrl)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as GR
import Gargantext.Sessions (OpenNodes, Session, mkNodeId, get)
import Gargantext.Types (ID, Reload, isPublic, publicize)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Forest.Tree"

------------------------------------------------------------------------
type CommonProps = (
    frontends     :: Frontends
  , handed        :: GT.Handed
  , mCurrentRoute :: Maybe AppRoute
  , openNodes     :: R.State OpenNodes
  , reload        :: R.State Reload
  , session       :: Session
  )

------------------------------------------------------------------------
type Props = (
    asyncTasks :: GAT.Reductor
  , root       :: ID
  | CommonProps
  )

treeView :: R2.Component Props
treeView = R.createElement elCpt
  where
    elCpt :: R.Component Props
    elCpt = R.hooksComponentWithModule thisModule "treeView" cpt

    cpt { asyncTasks
        , frontends
        , handed
        , mCurrentRoute
        , openNodes
        , reload
        , root
        , session
        } _children = do
      pure $ treeLoadView { asyncTasks
                          , frontends
                          , handed
                          , mCurrentRoute
                          , openNodes
                          , reload
                          , root
                          , session
                          } []

treeLoadView :: R2.Component Props
treeLoadView = R.createElement elCpt
  where
    elCpt :: R.Component Props
    elCpt = R.hooksComponentWithModule thisModule "treeLoadView" cpt

    cpt { asyncTasks
        , frontends
        , handed
        , mCurrentRoute
        , openNodes
        , reload
        , root
        , session
        } _children = do
      let fetch _ = getNodeTree session root
      -- let paint loaded = loadedTreeView { asyncTasks
      --                                   , frontends
      --                                   , handed
      --                                   , mCurrentRoute
      --                                   , openNodes
      --                                   , reload
      --                                   , session
      --                                   -- , tasks: tasksStruct root asyncTasks reload
      --                                   , tree: loaded
      --                                   } []
      let paint loaded = loadedTreeViewFirstLevel { asyncTasks
                                                  , frontends
                                                  , handed
                                                  , mCurrentRoute
                                                  , openNodes
                                                  , reload
                                                  , session
                                                  -- , tasks: tasksStruct root asyncTasks reload
                                                  , tree: loaded
                                                  } []
      useLoader { root, counter: fst reload } fetch paint

--------------
getNodeTree :: Session -> GT.ID -> Aff FTree
getNodeTree session nodeId = get session $ GR.NodeAPI GT.Tree (Just nodeId) ""
--------------
getNodeTreeFirstLevel :: Session -> GT.ID -> Aff FTree
getNodeTreeFirstLevel session nodeId = get session $ GR.TreeFirstLevel (Just nodeId) ""
--------------
type TreeViewProps = (
    asyncTasks :: GAT.Reductor
  , tree       :: FTree
  | CommonProps
  )

loadedTreeView :: R2.Component TreeViewProps
loadedTreeView = R.createElement elCpt
  where
    elCpt :: R.Component TreeViewProps
    elCpt = R.hooksComponentWithModule thisModule "loadedTreeView" cpt

    cpt { asyncTasks
        , frontends
        , handed
        , mCurrentRoute
        , openNodes
        , reload
        , session
        -- , tasks
        , tree
      } _ = do
      pure $ H.ul { className: "tree" } [
        H.div { className: if handed == GT.RightHanded then "righthanded" else "lefthanded" } [
          toHtml { asyncTasks
                 , frontends
                 , handed
                 , mCurrentRoute
                 , openNodes
                 , reload
                 , session
                   -- , tasks
                 , tree
                 } []
            ]
        ]

loadedTreeViewFirstLevel :: R2.Component TreeViewProps
loadedTreeViewFirstLevel = R.createElement elCpt
  where
    elCpt :: R.Component TreeViewProps
    elCpt = R.hooksComponentWithModule thisModule "loadedTreeViewFirstLevel" cpt

    cpt { asyncTasks
        , frontends
        , handed
        , mCurrentRoute
        , openNodes
        , reload
        , session
        -- , tasks
        , tree
      } _ = do
      pure $ H.ul { className: "tree" } [
        H.div { className: if handed == GT.RightHanded then "righthanded" else "lefthanded" } [
          toHtmlFirstLevel { asyncTasks
                           , frontends
                           , handed
                           , mCurrentRoute
                           , openNodes
                           , reload
                           , session
                             -- , tasks
                           , tree
                           } []
            ]
        ]

------------------------------------------------------------------------


type ToHtmlProps = (
    asyncTasks :: GAT.Reductor
  -- , tasks      :: Record Tasks
  , tree       :: FTree
  | CommonProps
  )

toHtml :: R2.Component ToHtmlProps
toHtml = R.createElement elCpt
  where
    elCpt :: R.Component ToHtmlProps
    elCpt = R.hooksComponentWithModule thisModule "toHtml" cpt

    cpt p@{ asyncTasks
          , frontends
          , handed
          , mCurrentRoute
          , openNodes
          , reload: reload@(_ /\ setReload)
          , session
          , tree: tree@(NTree (LNode { id
                                    , name
                                    , nodeType
                                    }
                              ) ary
                        )
          } _ = do
      let commonProps = RecordE.pick p :: Record CommonProps
      let pAction a   = performAction a (RecordE.pick p :: Record PerformActionProps)

      let nodeId               = mkNodeId session id
      let folderIsOpen         = Set.member nodeId (fst openNodes)
      let setFn                = if folderIsOpen then Set.delete else Set.insert
      let toggleFolderIsOpen _ = (snd openNodes) (setFn nodeId)
      let folderOpen           = Tuple folderIsOpen toggleFolderIsOpen

      let withId (NTree (LNode {id: id'}) _) = id'

      let publicizedChildren = if isPublic nodeType
                              then map (\t -> map (\(LNode n@{ nodeType: nt } )
                                                    -> (LNode (n { nodeType = publicize nt }))
                                                  ) t) ary
                              else ary

      pure $ H.li { className: if A.null ary then "no-children" else "with-children" }
        [ nodeSpan { appReload: reload
                  , asyncTasks
                  , dispatch: pAction
                  , folderOpen
                  , frontends
                  , handed
                  , id
                  , isLeaf: A.null ary
                  , mCurrentRoute
                  , name
                  , nodeType
                  , session
                  -- , tasks
                  }
            (
              childNodes ( Record.merge commonProps
                          { asyncTasks
                          , children: publicizedChildren
                          , folderOpen
                          , handed
                          }
                        )
            )
        ]

toHtmlFirstLevel :: R2.Component ToHtmlProps
toHtmlFirstLevel = R.createElement elCpt
  where
    elCpt :: R.Component ToHtmlProps
    elCpt = R.hooksComponentWithModule thisModule "toHtmlFirstLevel" cpt

    cpt p@{ asyncTasks
          , frontends
          , handed
          , mCurrentRoute
          , openNodes
          , reload: reload@(_ /\ setReload)
          , session
          , tree: tree@(NTree (LNode { id
                                      , name
                                      , nodeType
                                      }
                              ) ary
                        )
          } _ = do
      let pAction a   = performAction a (RecordE.pick p :: Record PerformActionProps)

      let nodeId               = mkNodeId session id
      let folderIsOpen         = Set.member nodeId (fst openNodes)
      let setFn                = if folderIsOpen then Set.delete else Set.insert
      let toggleFolderIsOpen _ = (snd openNodes) (setFn nodeId)
      let folderOpen           = Tuple folderIsOpen toggleFolderIsOpen

      let withId (NTree (LNode {id: id'}) _) = id'

      pure $ H.li { className: if A.null ary then "no-children" else "with-children" } $
        [ nodeSpan { appReload: reload
                    , asyncTasks
                    , dispatch: pAction
                    , folderOpen
                    , frontends
                    , handed
                    , id
                    , isLeaf: A.null ary
                    , mCurrentRoute
                    , name
                    , nodeType
                    , session
                    -- , tasks
                    }
          $ renderChildren folderOpen publicizedChildren
        ]
      where
        commonProps = RecordE.pick p :: Record CommonProps

        publicizedChildren = if isPublic nodeType
                              then map (\t -> map (\(LNode n@{ nodeType:nt } )
                                                  -> (LNode (n { nodeType = publicize nt }))
                                                  ) t) ary
                              else ary

        renderChildren (false /\ _) _ = []
        renderChildren folderOpen@(true /\ _) cs =
          (
            map (\t@(NTree (LNode {id: cId}) _) ->
                  childNodeFirstLevel ( Record.merge commonProps
                                        { asyncTasks
                                        , folderOpen
                                        , handed
                                        , id: cId
                                        }
                                      ) []
                ) publicizedChildren
          )
          -- childNodesFirstLevel ( Record.merge commonProps
          --                        { asyncTasks
          --                        , children: if isPublic nodeType
          --                                        then map (\t -> map (\(LNode n@{ nodeType:nt } )
          --                                                              -> (LNode (n { nodeType= publicize nt }))
          --                                                          ) t) ary
          --                                        else ary
          --                        , folderOpen
          --                        , handed
          --                        }
          --                      )


type ChildNodesProps =
  ( asyncTasks :: GAT.Reductor
  , children   :: Array FTree
  , folderOpen :: R.State Boolean
  | CommonProps
  )

childNodes :: Record ChildNodesProps -> Array R.Element
childNodes { children: []                       } = []
childNodes { folderOpen: (false /\ _)           } = []
childNodes props@{ asyncTasks, children, reload, handed } =
  map (\ctree@(NTree (LNode {id}) _) -> H.ul {} [
        toHtml (Record.merge commonProps { asyncTasks
                                         , handed
                                         -- , tasks: tasksStruct id asyncTasks reload
                                         , tree: ctree
                                         }
               ) []
        ]
      ) $ sorted children
  where
    commonProps = RecordE.pick props :: Record CommonProps
    sorted :: Array FTree -> Array FTree
    sorted = A.sortWith (\(NTree (LNode {id}) _) -> id)

type ChildNodeFirstLevelProps = (
    asyncTasks :: GAT.Reductor
  , folderOpen :: R.State Boolean
  , id         :: ID
  | CommonProps
  )

childNodeFirstLevel :: R2.Component ChildNodeFirstLevelProps
childNodeFirstLevel = R.createElement elCpt
  where
    elCpt :: R.Component ChildNodeFirstLevelProps
    elCpt = R.hooksComponentWithModule thisModule "childNodeFirstLevel" cpt

    cpt props@{ asyncTasks, folderOpen, id, frontends, handed, mCurrentRoute, openNodes, reload, session } _ = do
      cptReload <- R.useState' 0

      let fetch _ = getNodeTreeFirstLevel session id
      let paint loaded = childNodeFirstLevelPaint { asyncTasks
                                                  , folderOpen
                                                  , frontends
                                                  , handed
                                                  , mCurrentRoute
                                                  , openNodes
                                                  , reload: cptReload
                                                  , session
                                                  , tree: loaded } []

      useLoader { counter: fst cptReload, root: id } fetch paint


type ChildNodeFirstLevelPaintProps = (
    asyncTasks :: GAT.Reductor
  , folderOpen :: R.State Boolean
  , tree       :: FTree
  | CommonProps
  )

childNodeFirstLevelPaint :: R2.Component ChildNodeFirstLevelPaintProps
childNodeFirstLevelPaint = R.createElement elCpt
  where
    elCpt :: R.Component ChildNodeFirstLevelPaintProps
    elCpt = R.hooksComponentWithModule thisModule "childNodeFirstLevelPaint" cpt

    cpt props@{ asyncTasks, handed, folderOpen: (false /\ _), reload, tree: ctree@(NTree (LNode { id }) _) } _ = do
      pure $ H.ul {} [
        toHtmlFirstLevel (Record.merge commonProps { asyncTasks
                                                   , handed
                                                   , tree: ctree }
                        ) []
        ]
      -- pure $ H.div { } [ H.text $ "[closed] Node id " <> show id ]
      where
        commonProps = RecordE.pick props :: Record CommonProps
    cpt props@{ asyncTasks, handed, folderOpen: (true /\ _), reload, tree: ctree@(NTree (LNode { id }) _) } _ = do
      pure $ H.ul {} [
        toHtmlFirstLevel (Record.merge commonProps { asyncTasks
                                                   , handed
                                                   , tree: ctree }
                        ) []
        ]
      -- pure $ H.div { } [ H.text $ "[opened] Node id " <> show id ]
      where
        commonProps = RecordE.pick props :: Record CommonProps


type PerformActionProps =
  ( asyncTasks :: GAT.Reductor
  , openNodes :: R.State OpenNodes
  , reload    :: R.State Reload
  , session   :: Session
  -- , tasks     :: Record Tasks
  , tree      :: FTree
  )

-------
performAction :: Action
              -> Record PerformActionProps
              -> Aff Unit
performAction (DeleteNode nt) p@{ openNodes: (_ /\ setOpenNodes)
                                , reload: (_ /\ setReload)
                                , session
                                , tree: (NTree (LNode {id, parent_id}) _)
                                } =
  do
    case nt of
         GT.NodePublic GT.FolderPublic -> void $ deleteNode session nt id
         GT.NodePublic _               -> void $ unpublishNode session parent_id id
         _                             -> void $ deleteNode session nt id

    liftEffect $ setOpenNodes (Set.delete (mkNodeId session id))
    performAction RefreshTree p

-------
performAction (DoSearch task) { asyncTasks: (_ /\ dispatch)
                              , session
                              , tree: (NTree (LNode {id}) _)
                              }  =
  do
    liftEffect $ dispatch $ GAT.Insert id task
    liftEffect $ log2 "[performAction] DoSearch task:" task

-------
performAction (UpdateNode params) { asyncTasks: (_ /\ dispatch)
                                  , session
                                  , tree: (NTree (LNode {id}) _)
                                  } =
  do
    task <- updateRequest params session id
    liftEffect $ dispatch $ GAT.Insert id task
    liftEffect $ log2 "[performAction] UpdateNode task:" task


-------
performAction (RenameNode name) p@{ reload: (_ /\ setReload)
                                  , session
                                  , tree: (NTree (LNode {id}) _)
                                  } =
  do
    void $ rename session id $ RenameValue {text:name}
    performAction RefreshTree p

-------
performAction (ShareTeam username) p@{ reload: (_ /\ setReload)
                                     , session
                                     , tree: (NTree (LNode {id}) _)
                                     } =
  do
    void $ Share.shareReq session id $ Share.ShareTeamParams {username}


performAction (SharePublic {params}) p@{ session
                                       , openNodes: (_ /\ setOpenNodes)
                                       } =
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:inId,out}) -> do
      void $ Share.shareReq session inId $ Share.SharePublicParams {node_id:out}
      liftEffect $ setOpenNodes (Set.insert (mkNodeId session out))
      performAction RefreshTree p


performAction (AddContact params) p@{ reload: (_ /\ setReload)
                                     , session
                                     , tree: (NTree (LNode {id}) _)
                                     } =
    void $ Contact.contactReq session id params



-------
performAction (AddNode name nodeType) p@{ openNodes: (_ /\ setOpenNodes)
                                        , reload:    (_ /\ setReload)
                                        , session
                                        , tree: (NTree (LNode {id}) _)
                                        } =
  do
    task <- addNode session id $ AddNodeValue {name, nodeType}
    liftEffect $ setOpenNodes (Set.insert (mkNodeId session id))
    performAction RefreshTree p

-------
performAction (UploadFile nodeType fileType mName blob) { asyncTasks: (_ /\ dispatch)
                                                        , session
                                                        , tree: (NTree (LNode {id}) _)
                                                        } =
  do
    task <- uploadFile session nodeType id fileType {mName, blob}
    liftEffect $ dispatch $ GAT.Insert id task
    liftEffect $ log2 "Uploaded, task:" task

performAction (UploadArbitraryFile mName blob) { asyncTasks: (_ /\ dispatch)
                                               , session
                                               , tree: (NTree (LNode {id}) _)
                                               } =
  do
    task <- uploadArbitraryFile session id { blob, mName }
    liftEffect $ dispatch $ GAT.Insert id task
    liftEffect $ log2 "Uploaded, task:" task

-------
performAction DownloadNode _ = do
    liftEffect $ log "[performAction] DownloadNode"
-------
performAction (MoveNode {params}) p@{session} =
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:in',out}) -> do
      void $ moveNodeReq session in' out
      performAction RefreshTree p

performAction (MergeNode {params}) p@{session} =
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:in',out}) -> do
      void $ mergeNodeReq session in' out
      performAction RefreshTree p

performAction (LinkNode {nodeType, params}) p@{session} = do
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:in',out}) -> do
      void $ linkNodeReq session nodeType in' out
      performAction RefreshTree p

-------
performAction RefreshTree { reload: (_ /\ setReload) } = do
  liftEffect $ setReload (_ + 1)
-------
performAction NoAction _ = do
    liftEffect $ log "[performAction] NoAction"

