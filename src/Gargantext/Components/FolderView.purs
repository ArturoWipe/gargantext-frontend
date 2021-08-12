module Gargantext.Components.FolderView where

import Control.Monad.Error.Class (throwError)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (null)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (AddNodeValue(..), addNode)
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Delete (deleteNode, unpublishNode)
import Gargantext.Components.Forest.Tree.Node.Action.Link (linkNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Merge (mergeNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Move (moveNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (RenameValue(..), rename)
import Gargantext.Components.Forest.Tree.Node.Action.Share as Share
import Gargantext.Components.Forest.Tree.Node.Action.Update (updateRequest)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadArbitraryFile, uploadFile)
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..), fTreeID)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Config.REST (RESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Ordering, Unit, bind, compare, discard, pure, unit, void, ($), (<$>), (<>))
import Gargantext.Routes (AppRoute(Home), SessionRoute(..), appPath, nodeTypeAppRoute)
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (FrontendError, NodeType(..))
import Gargantext.Types as GT
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

foreign import back :: Effect Unit
foreign import link :: String -> Effect Unit

here :: R2.Here
here = R2.here "Gargantext.Components.FolderView"

type Props =
  ( backFolder   :: Boolean
  , errors       :: T.Box (Array FrontendError)
  , nodeId       :: Int
  , reloadForest :: T.Box T2.Reload
  , session      :: Session
  , tasks        :: T.Box GAT.Storage
  )

data FolderStyle = FolderUp | FolderChild

folderView :: R2.Leaf Props
folderView props = R.createElement folderViewCpt props []
folderViewCpt :: R.Component Props
folderViewCpt = here.component "folderViewCpt" cpt where
  cpt { errors, nodeId, session, backFolder, tasks, reloadForest } _ = do
    setPopoverRef <- R.useRef Nothing
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { errorHandler
              , loader: loadFolders
              , path: { nodeId, session, reload: reload'}
              , render: \folders -> folderViewMain { backFolder
                                                   , errors
                                                   , folders
                                                   , nodeId
                                                   , session
                                                   , tasks
                                                   , reload
                                                   , setPopoverRef
                                                   , reloadForest} }
    where
      errorHandler err = here.log2 "[folderView] RESTError" err

type FolderViewProps = 
  ( backFolder    :: Boolean
  , errors        :: T.Box (Array FrontendError)
  , folders       :: FTree
  , nodeId        :: Int
  , reload        :: T.Box T2.Reload
  , reloadForest  :: T.Box T2.Reload
  , session       :: Session
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  , tasks         :: T.Box GAT.Storage
  )

folderViewMain :: Record FolderViewProps -> R.Element
folderViewMain props = R.createElement folderViewMainCpt props []
folderViewMainCpt :: R.Component FolderViewProps
folderViewMainCpt = here.component "folderViewMainCpt" cpt where
  cpt { backFolder
      , errors
      , folders: NTree (LNode {parent_id: parentId, nodeType}) (folders)
      , nodeId
      , reload
      , reloadForest
      , session
      , setPopoverRef
      , tasks } _ = do
    let foldersS = A.sortBy sortFolders folders
    let backHome = isBackHome nodeType
    let parent = makeParentFolder parentId session backFolder backHome
    let children = makeFolderElements foldersS { errors, session, setPopoverRef, nodeId, tasks, reload, reloadForest }

    pure $ H.div {className: "fv folders"} $ parent <> children

  makeFolderElements foldersS props = makeFolderElementsMap <$> foldersS where
    makeFolderElementsMap :: NTree LNode -> R.Element
    makeFolderElementsMap (NTree (LNode node) _) = folder { errors: props.errors
                                                          , nodeId: node.id
                                                          , nodeType: node.nodeType
                                                          , parentId: props.nodeId
                                                          , tasks: props.tasks
                                                          , reload: props.reload
                                                          , reloadForest: props.reloadForest
                                                          , session: props.session
                                                          , setPopoverRef: props.setPopoverRef
                                                          , style: FolderChild
                                                          , text: node.name } []

  makeParentFolder :: Maybe Int -> Session -> Boolean -> Boolean -> Array R.Element
  makeParentFolder (Just parentId) session _ _ =
    -- FIXME: The NodeType here should not be hardcoded to FolderPrivate but we currently can't get the actual NodeType
    -- without performing another API call. Also parentId is never being returned by this API even when it clearly exists
    [ folderSimple {style: FolderUp, text: "..", nodeId: parentId, nodeType: GT.FolderPrivate, session: session} [] ]
  makeParentFolder Nothing _ _ true = [ H.a {className: "btn btn-primary", href: appPath Home} [ H.i { className: "fa fa-folder-open" } []
                                                                   , H.br {}
                                                                   , H.text ".."] ]
  makeParentFolder Nothing _ true _ = [ H.button {className: "btn btn-primary", on: { click: back } }  [ H.i { className: "fa fa-folder-open" } []
                                                                   , H.br {}
                                                                   , H.text ".."] ]
  makeParentFolder Nothing _ _ _ = []


  sortFolders :: FTree -> FTree -> Ordering
  sortFolders a b = compare (fTreeID a) (fTreeID b)

  isBackHome :: GT.NodeType -> Boolean
  isBackHome GT.FolderPrivate = true
  isBackHome GT.FolderPublic = true
  isBackHome GT.FolderShared = true
  isBackHome _ = false


type FolderSimpleProps = 
  (
    style :: FolderStyle
  , text :: String
  , nodeType :: GT.NodeType
  , nodeId :: Int
  , session :: Session
  )

folderSimple :: R2.Component FolderSimpleProps
folderSimple = R.createElement folderSimpleCpt

folderSimpleCpt :: R.Component FolderSimpleProps
folderSimpleCpt = here.component "folderSimpleCpt" cpt where
  cpt {style, text, nodeId, session, nodeType} _ = do
    let sid = sessionId session
    pure $ H.a { className: "btn btn-primary"
               , href: "/#/" <> getFolderPath nodeType sid nodeId }
      [ H.i { className: icon style nodeType } []
      , H.br {}
      , H.text text ]
  
  icon :: FolderStyle -> GT.NodeType -> String
  icon FolderUp _ = "fa fa-folder-open"
  icon _ nodeType = GT.fldr nodeType false

  getFolderPath :: GT.NodeType -> GT.SessionId -> Int -> String
  getFolderPath nodeType sid nodeId = appPath $ fromMaybe Home $ nodeTypeAppRoute nodeType sid nodeId

type FolderProps = 
  ( errors        :: T.Box (Array FrontendError)
  , parentId      :: Int
  , reload        :: T.Box T2.Reload
  , reloadForest  :: T.Box T2.Reload
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  , tasks         :: T.Box GAT.Storage
  | FolderSimpleProps
  )

folder :: R2.Component FolderProps
folder = R.createElement folderCpt
folderCpt :: R.Component FolderProps
folderCpt = here.component "folderCpt" cpt where
  cpt props@{ errors, style, text, nodeId, session, nodeType, setPopoverRef, parentId, tasks, reload, reloadForest } _ = do
    let sid = sessionId session
    let dispatch a = performAction a { errors, nodeId, parentId, tasks, reload, reloadForest, session, setPopoverRef }
    popoverRef <- R.useRef null

    R.useEffect' $ do
        R.setRef setPopoverRef $ Just $ Popover.setOpen popoverRef

    pure $ 
        H.div {} [
        H.span{style: {position: "absolute"}} [ Popover.popover {
            arrow: false
          , open: false
          , onClose: \_ -> pure unit
          , onOpen:  \_ -> pure unit
          , ref: popoverRef 
          } [
              popOverIcon
              , mNodePopupView (Record.merge props {dispatch}) (onPopoverClose popoverRef)
              ]]
      , H.button {on: {click: link ("/#/" <> getFolderPath nodeType sid nodeId) }, className: "btn btn-primary fv btn" } [ 
          H.i {className: icon style nodeType} []
        , H.br {}
        , H.text text]]
    
    
  icon :: FolderStyle -> GT.NodeType -> String
  icon FolderUp _ = "fa fa-folder-open"
  icon _ nodeType = GT.fldr nodeType false

  getFolderPath :: GT.NodeType -> GT.SessionId -> Int -> String
  getFolderPath nodeType sid nodeId = appPath $ fromMaybe Home $ nodeTypeAppRoute nodeType sid nodeId

  onPopoverClose popoverRef _ = Popover.setOpen popoverRef false

  popOverIcon = H.span { className: "fv action" } [
        H.a { className: "settings fa fa-cog" 
          , title : "Each node of the Tree can perform some actions.\n"
            <> "Click here to execute one of them." } []
      ]

  mNodePopupView props opc = nodePopupView { dispatch: props.dispatch
                                           , errors: props.errors
                                           , id: props.nodeId
                                           , onPopoverClose: opc
                                           , nodeType: props.nodeType
                                           , name: props.text
                                           , session: props.session
                                           , handed: GT.RightHanded
                                           }

backButton :: R.Element
backButton = 
  H.button {
    className: "btn btn-primary"
  , on: {click: back}
  } [
    H.i { className: "fa fa-arrow-left", title: "Previous view"} []
  ]

homeButton :: R.Element
homeButton =
  H.a {
    className: "btn btn-primary"
  , href: appPath Home
  } [
    H.i { className: "fa fa-home", title: "Back to home"} []
  ]

type LoadProps =
  (
    session :: Session,
    nodeId :: Int,
    reload :: T2.Reload
  )

loadFolders :: Record LoadProps -> Aff (Either RESTError FTree)
loadFolders {nodeId, session} = get session $ TreeFirstLevel (Just nodeId) ""

type PerformActionProps =
  ( errors       :: T.Box (Array FrontendError)
  , nodeId        :: Int
  , parentId      :: Int
  , reload        :: T.Box T2.Reload
  , reloadForest  :: T.Box T2.Reload
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  , session       :: Session
  , tasks         :: T.Box GAT.Storage
  )

performAction :: Action -> Record PerformActionProps -> Aff Unit
performAction = performAction' where
  performAction' (DeleteNode nt) p = deleteNode' nt p
  performAction' (DoSearch task) p = doSearch task p
  performAction' (UpdateNode params) p = updateNode params p
  performAction' (RenameNode name) p = renameNode name p
  performAction' (ShareTeam username) p = shareTeam username p
  performAction' (SharePublic { params }) p = sharePublic params p
  performAction' (AddContact params) p = addContact params p
  performAction' (AddNode name nodeType) p = addNode' name nodeType p
  performAction' (UploadFile nodeType fileType mName contents) p = uploadFile' nodeType fileType mName contents p
  performAction' (UploadArbitraryFile mName blob) p = uploadArbitraryFile' mName blob p
  performAction' DownloadNode _ = liftEffect $ here.log "[performAction] DownloadNode"
  performAction' (MoveNode {params}) p = moveNode params p
  performAction' (MergeNode {params}) p = mergeNode params p
  performAction' (LinkNode { nodeType, params }) p = linkNode nodeType params p
  performAction' NoAction _ = liftEffect $ here.log "[performAction] NoAction"
  performAction' ClosePopover p = closePopover p
  performAction' _ _ = liftEffect $ here.log "[performAction] unsupported action"

  closePopover { setPopoverRef } =
    liftEffect $ traverse_ (\set -> set false) (R.readRef setPopoverRef)

  refreshFolders p = do 
    liftEffect $ T2.reload p.reload
    liftEffect $ T2.reload p.reloadForest
    closePopover p

  deleteNode' nt p@{ nodeId: id, parentId: parent_id } = do
    case nt of
      NodePublic FolderPublic  -> void $ deleteNode p.session nt id
      NodePublic _             -> void $ unpublishNode p.session (Just parent_id) id
      _                        -> void $ deleteNode p.session nt id
    refreshFolders p

  doSearch task { tasks, nodeId: id } = liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[performAction] DoSearch task:" task

  updateNode params p@{ errors, tasks, nodeId: id } = do
    eTask <- updateRequest params p.session id
    handleRESTError errors eTask $ \task -> liftEffect $ do
      GAT.insert id task tasks
      here.log2 "[performAction] UpdateNode task:" task
  
  shareTeam username p@{ errors, nodeId: id} = do
    eTask <- Share.shareReq p.session id $ Share.ShareTeamParams {username}
    handleRESTError errors eTask $ \_task -> pure unit

  sharePublic params p@{ errors } = traverse_ f params where
    f (SubTreeOut { in: inId, out }) = do
      eTask <- Share.shareReq p.session inId $ Share.SharePublicParams { node_id: out }
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  addContact params p@{ nodeId: id } =
    void $ Contact.contactReq p.session id params

  uploadFile' nodeType fileType mName contents p@{ errors, tasks, nodeId: id } = do
    eTask <- uploadFile { contents, fileType, id, nodeType, mName, session: p.session }
    handleRESTError errors eTask $ \task -> liftEffect $ do
      GAT.insert id task tasks
      here.log2 "[performAction] UploadFile, uploaded, task:" task

  uploadArbitraryFile' mName blob p@{ errors, tasks, nodeId: id } = do
    eTask <- uploadArbitraryFile p.session id { blob, mName }
    handleRESTError errors eTask $ \task -> liftEffect $ do
      GAT.insert id task tasks
      here.log2 "[performAction] UploadArbitraryFile, uploaded, task:" task

  moveNode params p@{ errors } = traverse_ f params where
    f (SubTreeOut { in: in', out }) = do
      eTask <- moveNodeReq p.session in' out
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  mergeNode params p@{ errors } = traverse_ f params where
    f (SubTreeOut { in: in', out }) = do
      eTask <- mergeNodeReq p.session in' out
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  linkNode nodeType params p@{ errors } = traverse_ f params where
    f (SubTreeOut { in: in', out }) = do
      eTask <- linkNodeReq p.session nodeType in' out
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  renameNode name p@{ errors, nodeId: id } = do
    eTask <- rename p.session id $ RenameValue { text: name }
    handleRESTError errors eTask $ \_task -> pure unit
    refreshFolders p

  addNode' name nodeType p@{ errors, nodeId: id } = do
    eTask <- addNode p.session id $ AddNodeValue {name, nodeType}
    handleRESTError errors eTask $ \_task -> pure unit
    refreshFolders p
