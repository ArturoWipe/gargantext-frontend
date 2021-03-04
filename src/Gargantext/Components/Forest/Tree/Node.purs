module Gargantext.Components.Forest.Tree.Node where

import Gargantext.Prelude

import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Symbol (SProxy(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T
import Web.HTML.Event.EventTypes (offline)

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Box.Types (CommonProps)
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Tools (nodeLink)
import Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.Forest.Tree.Node.Tools.Sync (nodeActionsGraph, nodeActionsNodeList)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (Lang(EN))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (Name, ID, reverseHanded)
import Gargantext.Types as GT
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Gargantext.Version as GV

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node"

-- Main Node
type NodeMainSpanProps =
  ( folderOpen    :: T.Cursor Boolean
  , frontends     :: Frontends
  , id            :: ID
  , isLeaf        :: IsLeaf
  , name          :: Name
  , nodeType      :: GT.NodeType
  , reloadRoot    :: T.Cursor T2.Reload
  , route         :: Routes.AppRoute
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  , tasks         :: T.Cursor (Maybe GAT.Reductor)
  | CommonProps
  )

type IsLeaf = Boolean

nodeSpan :: R2.Component NodeMainSpanProps
nodeSpan = R.createElement nodeSpanCpt

nodeSpanCpt :: R.Component NodeMainSpanProps
nodeSpanCpt = here.component "nodeSpan" cpt
  where
    cpt props children = do
      pure $ H.div {} ([ nodeMainSpan props [] ] <> children)

nodeMainSpan :: R2.Component NodeMainSpanProps
nodeMainSpan = R.createElement nodeMainSpanCpt

nodeMainSpanCpt :: R.Component NodeMainSpanProps
nodeMainSpanCpt = here.component "nodeMainSpan" cpt
  where
    cpt props@{ dispatch
              , folderOpen
              , frontends
              , handed
              , id
              , isLeaf
              , name
              , nodeType
              , reloadRoot
              , route
              , session
              , setPopoverRef
              , tasks
              } _ = do
      -- only 1 popup at a time is allowed to be opened
      droppedFile   <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver    <- R.useState' false
      popoverRef    <- R.useRef null
      R.useEffect' $ do
        R.setRef setPopoverRef $ Just $ Popover.setOpen popoverRef
      let isSelected = Just route == Routes.nodeTypeAppRoute nodeType (sessionId session) id

      tasks' <- T.read tasks

      pure $ H.span (dropProps droppedFile isDragOver)
        $ GT.reverseHanded
        [ folderIcon  { folderOpen, nodeType } []
        , chevronIcon { folderOpen, handed, isLeaf, nodeType } []
        , nodeLink { frontends, handed, folderOpen, id, isSelected
                   , name: name' props, nodeType, session } []

                , fileTypeView { dispatch, droppedFile, id, isDragOver, nodeType }
                , H.div {} (map (\t -> asyncProgressBar { asyncTask: t
                                                       , barType: Pie
                                                       , nodeId: id
                                                       , onFinish: onTaskFinish id t
                                                       , session
                                                       }
                                ) $ GAT.getTasksMaybe tasks' id
                           )
                , if nodeType == GT.NodeUser
                        then GV.versionView {session}
                        else H.div {} []

                , if showBox then
                        Popover.popover { arrow: false
                                        , open: false
                                        , onClose: \_ -> pure unit
                                        , onOpen:  \_ -> pure unit
                                        , ref: popoverRef } [
                        popOverIcon
                        , mNodePopupView props (onPopoverClose popoverRef)
                        ]
                else H.div {} []

                , nodeActions { id
                              , nodeType
                              , refresh: const $ dispatch RefreshTree
                              , session
                              } []
                ] handed
        where
          onTaskFinish id t _ = do
            mT <- T.read tasks
            case mT of
              Just t' -> snd t' $ GAT.Finish id t
              Nothing -> pure unit
            T2.reload reloadRoot

          SettingsBox {show: showBox} = settingsBox nodeType
          onPopoverClose popoverRef _ = Popover.setOpen popoverRef false

          name' {name, nodeType} = if nodeType == GT.NodeUser then show session else name

          mNodePopupView props@{id, nodeType} onPopoverClose =
                                nodePopupView { dispatch
                                              , handed : props.handed
                                              , id
                                              , name: name' props
                                              , nodeType
                                              , onPopoverClose
                                              , session
                                              }

    popOverIcon =
      H.a { className: "settings fa fa-cog" 
          , title : "Each node of the Tree can perform some actions.\n"
            <> "Click here to execute one of them." } []
    dropProps droppedFile isDragOver =
      { className: "leaf " <> (dropClass droppedFile isDragOver)
      , on: { drop: dropHandler droppedFile
            , dragOver: onDragOverHandler isDragOver
            , dragLeave: onDragLeave isDragOver }
      }
      where
        dropClass   (Just _  /\ _)        _          = "file-dropped"
        dropClass    _                   (true /\ _) = "file-dropped"
        dropClass   (Nothing /\ _)        _          = ""
        dropHandler (_ /\ setDroppedFile) e          = do
          -- prevent redirection when file is dropped
          E.preventDefault e
          E.stopPropagation e
          blob <- R2.dataTransferFileBlob e
          void $ launchAff do
            --contents <- readAsText blob
            liftEffect $ setDroppedFile
                       $ const
                       $ Just
                       $ DroppedFile { blob: (UploadFileBlob blob)
                                     , fileType: Just CSV
                                     , lang    : EN
                                     }
    onDragOverHandler (_ /\ setIsDragOver) e = do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e
      setIsDragOver $ const true
    onDragLeave (_ /\ setIsDragOver) _ = setIsDragOver $ const false

type FolderIconProps = (
    folderOpen :: T.Cursor Boolean
  , nodeType   ::  GT.NodeType
  )

folderIcon :: R2.Component FolderIconProps
folderIcon = R.createElement folderIconCpt

folderIconCpt :: R.Component FolderIconProps
folderIconCpt = here.component "folderIcon" cpt
  where
    cpt { folderOpen, nodeType } _ = do
      open <- T.read folderOpen
      pure $ H.a { className: "folder-icon", on: { click: \_ -> T.modify not folderOpen } }
        [ H.i { className: GT.fldr nodeType open } [] ]

type ChevronIconProps = (
    folderOpen :: T.Cursor Boolean
  , handed     :: GT.Handed
  , isLeaf     :: Boolean
  , nodeType   :: GT.NodeType
  )

chevronIcon :: R2.Component ChevronIconProps
chevronIcon = R.createElement chevronIconCpt

chevronIconCpt :: R.Component ChevronIconProps
chevronIconCpt = here.component "chevronIcon" cpt
  where
    cpt { folderOpen, handed, isLeaf: true, nodeType } _ = do
      pure $ H.div {} []
    cpt { folderOpen, handed, isLeaf: false, nodeType } _ = do
      open <- T.read folderOpen
      pure $ H.a { className: "chevron-icon"
          , on: { click: \_ -> T.modify not folderOpen }
          }
        [ H.i { className: if open
                            then "fa fa-chevron-down"
                            else if handed == GT.RightHanded
                                    then "fa fa-chevron-right"
                                    else "fa fa-chevron-left"
                } [] ]

{-
fldr nt open = if open
               then "fa fa-globe" -- <> color nt
               else "fa fa-folder-globe" -- <> color nt
               --else "fa fa-folder-close" <> color nt
                 where
                   color GT.NodeUser     = ""
                   color FolderPublic = ""
                   color FolderShared = " text-warning"
                   color _            = " text-danger"
-}


-- START nodeActions

type NodeActionsCommon =
  ( id       :: ID
  , refresh  :: Unit -> Aff Unit
  , session  :: Session
  )

type NodeActionsProps = ( nodeType :: GT.NodeType | NodeActionsCommon )

nodeActions :: R2.Component NodeActionsProps
nodeActions = R.createElement nodeActionsCpt

nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = here.component "nodeActions" cpt where
  cpt props _ = pure (child props.nodeType) where
    nodeActionsP = SProxy :: SProxy "nodeType"
    childProps = Record.delete nodeActionsP props
    child GT.NodeList = listNodeActions childProps
    child GT.Graph = graphNodeActions childProps
    child _ = H.div {} []

graphNodeActions :: R2.Leaf NodeActionsCommon
graphNodeActions props = R.createElement graphNodeActionsCpt props []

graphNodeActionsCpt :: R.Component NodeActionsCommon
graphNodeActionsCpt = here.component "graphNodeActions" cpt where
  cpt { id, session, refresh } _ =
    useLoader id (graphVersions session) $ \gv ->
      nodeActionsGraph { graphVersions: gv, session, id, refresh }
  graphVersions session graphId = GraphAPI.graphVersions { graphId, session }

listNodeActions :: R2.Leaf NodeActionsCommon
listNodeActions props = R.createElement listNodeActionsCpt props []

listNodeActionsCpt :: R.Component NodeActionsCommon
listNodeActionsCpt = here.component "listNodeActions" cpt where
  cpt { id, session, refresh } _ =
    useLoader { nodeId: id, session } loadCorpusWithChild $ \{ corpusId } ->
      nodeActionsNodeList
      { listId: id, nodeId: corpusId, session, refresh: refresh
      , nodeType: GT.TabNgramType GT.CTabTerms }

