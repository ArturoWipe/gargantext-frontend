module Gargantext.Components.Forest.Tree.Node where

import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Box.Types (CommonProps)
import Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.Forest.Tree.Node.Tools.Task (Tasks)
import Gargantext.Components.Forest.Tree.Node.Tools.Sync (nodeActionsGraph, nodeActionsNodeList)
import Gargantext.Components.Forest.Tree.Node.Tools (nodeLink)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (Lang(EN))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, const, discard, map, pure, show, unit, void, ($), (<>), (==), identity)
import Gargantext.Routes as Routes
import Gargantext.Version as GV
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (Name, ID)
import Gargantext.Types as GT
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Forest.Tree.Node"


-- Main Node
type NodeMainSpanProps =
  ( id            :: ID
  , folderOpen    :: R.State Boolean
  , frontends     :: Frontends
  , mCurrentRoute :: Maybe Routes.AppRoute
  , name          :: Name
  , nodeType      :: GT.NodeType
  , tasks         :: Record Tasks
  | CommonProps
  )

type IsLeaf = Boolean

nodeMainSpan :: IsLeaf
            -> Record NodeMainSpanProps
            -> R.Element
nodeMainSpan isLeaf p@{ dispatch, folderOpen, frontends, handed, session } = R.createElement el p []
  where
    el = R2.hooksComponent thisModule "nodeMainSpan" cpt
    cpt props@{id, mCurrentRoute, name, nodeType, tasks: { onTaskFinish, tasks }} _ = do
      -- only 1 popup at a time is allowed to be opened
      droppedFile   <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver    <- R.useState' false

      popoverRef    <- R.useRef null

      let ordering =
            case handed of
              GT.LeftHanded  -> reverse
              GT.RightHanded -> identity

      pure $ H.span (dropProps droppedFile isDragOver)
                $ ordering
                [ folderIcon  nodeType folderOpen
                , chevronIcon isLeaf handed nodeType folderOpen
                , nodeLink { frontends
                                , id
                                , isSelected: mCurrentRoute
                                        == Routes.nodeTypeAppRoute
                                                nodeType
                                                (sessionId session) id
                                , name: name' props
                                , nodeType
                                , session
                                , handed
                                }

                , fileTypeView { dispatch, droppedFile, id, isDragOver, nodeType }
                , H.div {} (map (\t -> asyncProgressBar { asyncTask: t
                                                        , barType: Pie
                                                        , corpusId: id
                                                        , onFinish: const $ onTaskFinish t
                                                        , session
                                                        }
                                ) tasks
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
                                , refreshTree: const $ dispatch RefreshTree
                                , session
                                }


                ]
               where
                 SettingsBox {show: showBox} = settingsBox nodeType
                 onPopoverClose popoverRef _ = Popover.setOpen popoverRef false

    name' {name, nodeType} = if nodeType == GT.NodeUser
                                then show session
                                else name

    chevronIcon isLeaf handed' nodeType folderOpen'@(open /\ _) =
      if isLeaf
         then H.div {} []
         else
           H.a { className: "chevron-icon"
               , onClick: R2.effToggler folderOpen'
               }
               [ H.i {
                    className: if open
                               then "fa fa-chevron-down"
                               else if handed' == GT.RightHanded
                                      then "fa fa-chevron-right"
                                      else "fa fa-chevron-left"
                    } [] ]

    folderIcon nodeType folderOpen'@(open /\ _) =
      H.a { className: "folder-icon"
          , onClick: R2.effToggler folderOpen'
          } [
        H.i {className: GT.fldr nodeType open} []
        ]

    popOverIcon = H.a { className: "settings fa fa-cog" 
                      , title : "Each node of the Tree can perform some actions.\n"
                             <> "Click here to execute one of them."
                      } []

    mNodePopupView props@{id, nodeType} onPopoverClose =
      nodePopupView { id
                    , dispatch
                    , name: name' props
                    , nodeType
                    , onPopoverClose
                    , session
                    , handed : props.handed
                    }

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

{-
fldr nt open = if open
               then "fa fa-globe" -- <> color nt
               else "fa fa-folder-globe" -- <> color nt
               --else "glyphicon glyphicon-folder-close" <> color nt
                 where
                   color GT.NodeUser     = ""
                   color FolderPublic = ""
                   color FolderShared = " text-warning"
                   color _            = " text-danger"
-}


-- START nodeActions

type NodeActionsProps =
  ( id          :: ID
  , nodeType    :: GT.NodeType
  , refreshTree :: Unit -> Aff Unit
  , session     :: Session
  )

nodeActions :: Record NodeActionsProps -> R.Element
nodeActions p = R.createElement nodeActionsCpt p []

nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = R2.hooksComponent thisModule "nodeActions" cpt
  where
    cpt { id
        , nodeType: GT.Graph
        , refreshTree
        , session
        } _ = do

      useLoader id (graphVersions session) $ \gv ->
        nodeActionsGraph { id
                         , graphVersions: gv
                         , session
                         , triggerRefresh: triggerRefresh refreshTree
                         }

    cpt { id
        , nodeType: GT.NodeList
        , refreshTree
        , session
        } _ = do
      useLoader { nodeId: id, session } loadCorpusWithChild $
        \{ corpusId } ->
          nodeActionsNodeList { listId: id
                              , nodeId: corpusId
                              , nodeType: GT.TabNgramType GT.CTabTerms
                              , session
                              , triggerRefresh: triggerRefresh refreshTree
                              }
    cpt _ _ = do
      pure $ H.div {} []

    graphVersions session graphId = GraphAPI.graphVersions { graphId, session }
    triggerRefresh refreshTree    = refreshTree


-- END nodeActions
