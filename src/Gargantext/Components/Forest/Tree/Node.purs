module Gargantext.Components.Forest.Tree.Node
  ( mainLeaf
  , blankMainLeaf
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log)
import Data.Array.NonEmpty as NArray
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.String.Regex as Regex
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), TooltipEffect(..), Variant(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Tools.Sync (nodeActionsGraph, nodeActionsNodeList)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (Lang(EN))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Config.REST (logRESTError')
import Gargantext.Context.Progress (asyncContext, asyncProgress)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Version (Version, useVersion)
import Gargantext.Plugins.Core.Console as C
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (ID, Name)
import Gargantext.Types as GT
import Gargantext.Utils (nbsp, textEllipsisBreak, (?))
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import React.SyntheticEvent as SE
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

-- (?) never been able to properly declare PureScript Regex...
foreign import nodeUserRegexp :: Regex.Regex

moduleName :: R2.Module
moduleName = "Gargantext.Components.Forest.Tree.Node"

console :: C.Console
console = C.encloseContext C.Component "Forest.Tree.Node"

-- Main Node
type MainLeafProps =
  ( boxes         :: Boxes
  , dispatch      :: Action -> Aff Unit
  , folderOpen    :: T.Box Boolean
  , frontends     :: Frontends
  , id            :: ID
  , isLeaf        :: Boolean
  , name          :: Name
  , nodeType      :: GT.NodeType
  , reload        :: T2.ReloadS
  , root          :: ID
  , session       :: Session
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  )

mainLeaf :: B.Leaf MainLeafProps
mainLeaf = B.leaf (moduleName <> "mainLeaf") cpt where
  cpt props@{ boxes: boxes@{ errors
                            , reloadMainPage
                            , reloadRoot
                            , route
                            , tasks }
            , dispatch
            , folderOpen
            , frontends
            , id
            , isLeaf
            , nodeType
            , reload
            , session
            , setPopoverRef
            } _ = do
  -- States

    route' <- T.useLive T.unequal route
    -- only 1 popup at a time is allowed to be opened
    droppedFile   <- T.useBox (Nothing :: Maybe DroppedFile)
    droppedFile'  <- T.useLive T.unequal droppedFile
    isDragOver    <- T.useBox false
    isDragOver'   <- T.useLive T.unequal isDragOver
    popoverRef    <- R.useRef null

    currentTasks <- GAT.focus id tasks
    currentTasks' <- T.useLive T.unequal currentTasks

    folderOpen' <- R2.useLive' folderOpen

    -- tasks' <- T.read tasks

  -- Computed
    let

      dropClass :: Maybe DroppedFile -> Boolean -> String
      dropClass (Just _) _    = "mainleaf--file-dropped"
      dropClass _        true = "mainleaf--file-dropped"
      dropClass Nothing  _    = ""

      name' :: String -> GT.NodeType -> Session -> String
      name' _ GT.NodeUser s = show s
      name' n _           _ = n

      isSelected = Just route' == Routes.nodeTypeAppRoute nodeType (sessionId session) id

      SettingsBox {show: showBox} = settingsBox nodeType

      href = url frontends $ GT.NodePath (sessionId session) nodeType (Just id)

      name = name' props.name nodeType session

  -- Methods

      dropHandler :: forall event.
            SE.SyntheticEvent_ event
        -> Effect Unit
      dropHandler e = do
        -- prevent redirection when file is dropped
        SE.preventDefault e
        SE.stopPropagation e
        blob <- R2.dataTransferFileBlob e
        void $ launchAff do
          --contents <- readAsText blob
          liftEffect $ do
            T.write_ (Just
                    $ DroppedFile { blob: (UploadFileBlob blob)
                                  , fileType: Just CSV
                                  , lang    : EN
                                  }) droppedFile

      onDragOverHandler :: forall event.
            T.Box Boolean
        -> SE.SyntheticEvent_ event
        -> Effect Unit
      onDragOverHandler box e = do
        -- prevent redirection when file is dropped
        -- https://stackoverflow.com/a/6756680/941471
        SE.preventDefault e
        SE.stopPropagation e
        T.write_ true box

      onDragLeave :: forall event.
            T.Box Boolean
        -> SE.SyntheticEvent_ event
        -> Effect Unit
      onDragLeave box _ = T.write_ false box

      onTaskFinish ::
            GT.NodeID
        -> GT.AsyncTaskWithType
        -> Unit
        -> Effect Unit
      onTaskFinish id' t _ = do
        GAT.finish id' t tasks
        if GAT.asyncTaskTTriggersAppReload t then do
          console.log2 "reloading root for task" t
          T2.reload reloadRoot
        else do
          if GAT.asyncTaskTTriggersTreeReload t then do
            console.log2 "reloading tree for task" t
            T2.reload reload
          else do
            console.log2 "task doesn't trigger a tree reload" t
            pure unit
          if GAT.asyncTaskTTriggersMainPageReload t then do
            console.log2 "reloading main page for task" t
            T2.reload reloadMainPage
          else do
            console.log2 "task doesn't trigger a main page reload" t
            pure unit
        -- snd tasks $ GAT.Finish id' t
        -- mT <- T.read tasks
        -- case mT of
        --   Just t' -> snd t' $ GAT.Finish id' t
        --   Nothing -> pure unit
        -- T2.reload reloadRoot

      onPopoverClose ::
            Popover.PopoverRef
        -> Effect Unit
      onPopoverClose ref = Popover.setOpen ref false

      -- NOTE Don't toggle tree if it is not selected
      onNodeLinkClick :: Unit -> Effect Unit
      onNodeLinkClick _ = when (not isSelected) (T.write_ true folderOpen)

  -- Hooks

    useFirstEffect' $
      R.setRef setPopoverRef $ Just $ Popover.setOpen popoverRef

    mVersion <- useVersion $ nodeType == GT.NodeUser ?
      Just { session } $
      Nothing

    host <- R2.getPortalHost

  -- Render

    pure $

      H.span
      { className: intercalate " "
          [ "mainleaf"
          , dropClass droppedFile' isDragOver'
          , isSelected ? "mainleaf--selected" $ ""
          ]
      , on: { dragLeave: onDragLeave isDragOver
            , dragOver: onDragOverHandler isDragOver
            , drop: dropHandler
            }
      }
      [

    -- // Abstract informations //

        nodeTooltip
        { id
        , nodeType
        , name
        }
        [
          case mVersion of
            Nothing -> mempty
            Just v  -> versionComparator v
        ]
      ,
        R.createPortal
        [
          fileTypeView
          { dispatch, droppedFile, id, isDragOver, nodeType } []
        ]
        host
      ,

    -- // Leaf informations data //

        folderIcon
        { isLeaf
        , isOpened: folderOpen'
        , callback: const $ T.modify_ (not) folderOpen
        }
      ,
        nodeIcon
        (
          { nodeType
          , isLeaf
          , callback: const $ T.modify_ (not) folderOpen
          , isSelected
          }
        )
        [
          case mVersion of
            Nothing                              -> mempty
            Just { clientVersion, remoteVersion} ->
              B.iconButton $
              { className: intercalate " "
                  [ "mainleaf__version-badge"
                  , clientVersion == remoteVersion ?
                      "mainleaf__version-badge--matched" $
                      "mainleaf__version-badge--mismatched"
                  ]
              , name: clientVersion == remoteVersion ?
                  "check-circle" $
                  "exclamation-circle"
              , callback: const $ T.modify_ (not) folderOpen
              }
        ]
      ,
        nodeLink
        { callback: onNodeLinkClick
        , href
        , id
        , name: name' props.name nodeType session
        , type: nodeType
        }
      ,

    -- // Leaf action features //

        nodeActions
        { id
        , nodeType
        , refresh: const $ dispatch RefreshTree
        , session
        } []
      ,
        B.if' (showBox) $

          Popover.popover
          { arrow: false
          , open: false
          , onClose: \_ -> pure unit
          , onOpen:  \_ -> pure unit
          , ref: popoverRef
          }
          [
            B.iconButton
            { name: "cog"
            , className: "mainleaf__settings-icon"
            -- (cf. Popover callbacks)
            , callback: const R.nothing
            , title:
                  "Each node of the Tree can perform some actions.\n"
                <> "Click here to execute one of them."
            , variant: Secondary
            , overlay: true
            }
          ,
            nodePopupView
            { boxes
            , dispatch
            , id
            , name
            , nodeType
            , onPopoverClose: const $ onPopoverClose popoverRef
            , session
            }
          ]
      ,
        R.fragment $ flip map currentTasks' \task ->

          asyncProgress
          { asyncTask: task
          , errors
          , nodeId: id
          , onFinish: onTaskFinish id task
          , session
          }
          [
            taskProgress
            {}
          ]
      ]


---------------------------------------------------------

type NodeIconProps =
  ( nodeType      ::  GT.NodeType
  , callback      :: Unit -> Effect Unit
  , isLeaf        :: Boolean
  , isSelected    :: Boolean
  )

nodeIcon :: B.Tree NodeIconProps
nodeIcon = B.tree (moduleName <> "nodeIcon") cpt where
  cpt { nodeType
      , callback
      , isLeaf
      , isSelected
      } children = do
    -- Render
    pure $

      H.span
      { className: "mainleaf__node-icon" } $
      [
        B.iconButton
        { name: GT.getIcon nodeType true
        , callback
        , status: isLeaf ? Idled $ Enabled
        , variant: isSelected ? Primary $ Dark
        }
      ]
        <> children

-----------------------------------------------

type FolderIconProps =
  ( isOpened :: Boolean
  , callback :: Unit -> Effect Unit
  , isLeaf   :: Boolean
  )

folderIcon :: B.Leaf FolderIconProps
folderIcon = B.leaf (moduleName <> "folderIcon") cpt where
  cpt { isLeaf: true } _ = pure $
    B.icon
    { className: intercalate " "
        ["mainleaf__folder-icon"
        , "mainleaf__folder-icon--leaf"
        ]
    , name: "caret-right"
    }

  cpt { callback, isOpened } _ = pure $

    B.iconButton
    { className: "mainleaf__folder-icon"
    , name: isOpened ? "caret-down" $ "caret-right"
    , callback
    }

-----------------------------------------------

type NodeLinkProps =
  ( callback   :: Unit -> Effect Unit
  , href       :: String
  , id         :: Int
  , name       :: GT.Name
  , type       :: GT.NodeType
  )

nodeLink :: B.Leaf NodeLinkProps
nodeLink = B.leaf (moduleName <> "nodeLink") cpt where
  cpt { callback
      , href
      , id
      , name
      , type: nodeType
      } _ = do
    -- Computed
    let
      tid = tooltipId name id

      aProps =
        { href
        } `Record.merge` B.tooltipBind tid

    -- Render
    pure $

      H.div
      { className: "mainleaf__node-link"
      , on: { click: const $ callback unit }
      }
      [
        H.a
        aProps
        [
          B.span_ $ nodeLinkText nodeType name
        ]
      ]

nodeLinkText :: GT.NodeType -> String -> String
nodeLinkText GT.NodeUser s = s # (truncateNodeUser)
                         >>> maybe s identity
nodeLinkText _           s = textEllipsisBreak 15 s

truncateNodeUser :: String -> Maybe String
truncateNodeUser = Regex.match (nodeUserRegexp) >=> flip NArray.index 1 >>> join

---------------------------------------------------

type NodeTooltipProps =
  ( name      :: String
  , id        :: GT.NodeID
  , nodeType  :: GT.NodeType
  )

nodeTooltip :: B.Tree NodeTooltipProps
nodeTooltip = B.tree (moduleName <> "nodeTooltip") cpt where
  cpt { name, id, nodeType } children = pure $

    B.tooltip
    { id: tooltipId name id
    , effect: FloatEffect
    , delayShow: 600
    } $
    [
      H.b
      {}
      [
        B.icon
        { name: GT.getIcon nodeType true }
      ,
        B.span_ $
          GT.prettyNodeType nodeType
      ]
    ,
      B.div_ $
        name
    ]
      <> children

tooltipId :: String -> GT.NodeID -> String
tooltipId name id = name <> "-node-link-" <> show id

---------------------------------------------------------

blankMainLeaf :: B.Leaf ()
blankMainLeaf = B.leaf (moduleName <> "__blank__") cpt where
  cpt _ _ = pure $

    H.div { className: "mainleaf mainleaf--blank" }
    [
      B.icon { className: "mainleaf__folder-icon", name: "caret-right"}
    ,
      H.span { className: "mainleaf__node-icon" }
      [
        B.icon { name: "circle" }
      ]
    ,
      H.div { className: "mainleaf__node-link"} [ H.text $ nbsp 1 ]
    ]

-----------------------------------------------

-- START nodeActions

type NodeActionsCommon =
  ( id       :: ID
  , refresh  :: Unit -> Aff Unit
  , session  :: Session
  )

type NodeActionsProps = ( nodeType :: GT.NodeType | NodeActionsCommon )

nodeActions :: B.Tree NodeActionsProps
nodeActions = B.tree (moduleName <> "nodeActions") cpt where
  cpt props@{ nodeType: GT.NodeList } _ =
    pure $ listNodeActions (childProps props)

  cpt props@{ nodeType: GT.Graph    } _ =
    pure $ graphNodeActions (childProps props)

  cpt _                               _ =
    pure $ mempty

  childProps p = Record.delete nodeActionsP p

  nodeActionsP      = SProxy :: SProxy "nodeType"

  -- cpt props _ = pure (child props.nodeType)
  --   where
  --     nodeActionsP      = SProxy :: SProxy "nodeType"

  --     childProps        = Record.delete nodeActionsP props

  --     child GT.NodeList = listNodeActions childProps
  --     child GT.Graph    = graphNodeActions childProps
  --     child _           = mempty

graphNodeActions :: B.Leaf NodeActionsCommon
graphNodeActions = B.leaf' (moduleName <> "graphNodeActions") cpt where
  cpt { id, session, refresh } _ = do
    -- States
    state /\ stateBox <- R2.useBox' Nothing

    -- Hooks
    useLoaderEffect
      { errorHandler
      , loader: graphVersions session
      , path: id
      , state: stateBox
      }

    -- Render
    pure $ B.fromMaybe_ state \gv ->

      nodeActionsGraph
      { graphVersions: gv, session, id, refresh }
      []

  graphVersions session graphId = GraphAPI.graphVersions { graphId, session }
  errorHandler = logRESTError' console "[graphNodeActions]"


listNodeActions :: B.Leaf NodeActionsCommon
listNodeActions = B.leaf (moduleName <> "listNodeActions") cpt where
  cpt { id, session, refresh } _ = do
    -- States
    state /\ stateBox <- R2.useBox' Nothing

    -- Hooks
    useLoaderEffect
      { errorHandler
      , loader: loadCorpusWithChild
      , path: { nodeId: id, session }
      , state: stateBox
      }

    -- Render
    pure $ B.fromMaybe_ state \{ corpusId } ->

      nodeActionsNodeList
      { listId: id
      , nodeId: corpusId
      , session
      , refresh: refresh
      , nodeType: GT.TabNgramType GT.CTabTerms
      }

    where
      errorHandler = logRESTError' console "[listNodeActions]"

-----------------------------------------------

type VersionComparatorProps =
  ( clientVersion :: Version
  , remoteVersion :: Version
  )

versionComparator :: B.Leaf VersionComparatorProps
versionComparator = B.leaf (moduleName <> "versionComparator") cpt where
  cpt { clientVersion, remoteVersion } _
    | clientVersion == remoteVersion = pure $
        B.caveat
        { variant: Success
        , className: "mainleaf__version-comparator"
        }
        [
          B.b_ "Versions match"
        ,
          content clientVersion remoteVersion
        ]
    | otherwise = pure $
        B.caveat
        { variant: Warning
        , className: "mainleaf__version-comparator"
        }
        [
          B.b_ "Versions mismatch"
        ,
          content clientVersion remoteVersion
        ]

  content :: Version -> Version -> R.Element
  content clientVersion remoteVersion =
    H.ul
    {}
    [
      H.li
      {}
      [
        B.span_ "frontend: "
      ,
        H.text $ nbsp 1
      ,
        B.code_ clientVersion
      ]
    ,
      H.li
      {}
      [
        B.span_ "backend: "
      ,
        H.text $ nbsp 1
      ,
        B.code_ remoteVersion
      ]
    ]

-------------------------------------------------------

taskProgress :: B.Leaf ()
taskProgress = B.leaf (moduleName <> "progress") cpt where
  cpt _ _ = do
    -- Context
    asyncProgressContext <- R.useContext asyncContext
    -- Render
    pure $

      H.span
      { className: "mainleaf__progress-bar" }
      [
        B.progressBar
        { value: asyncProgressContext
        , variant: Info
        }
      ]
