module Gargantext.Components.Forest.Tree.Node where

import Gargantext.Prelude

import DOM.Simple as DOM
import DOM.Simple.Event as DE
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.Forest.Tree.Node.Tools.Sync (nodeActionsGraph, nodeActionsNodeList)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (Lang(EN))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (ID, Name)
import Gargantext.Types as GT
import Gargantext.Utils (textEllipsisBreak, (?))
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.ReactTooltip as ReactTooltip
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Gargantext.Version as GV
import React.SyntheticEvent (SyntheticEvent_)
import React.SyntheticEvent as E
import React.SyntheticEvent as SE
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node"

-- Main Node
type NodeSpanProps =
  ( boxes         :: Boxes
  , dispatch      :: Action -> Aff Unit
  , folderOpen    :: T.Box Boolean
  , frontends     :: Frontends
  , id            :: ID
  , isLeaf        :: IsLeaf
  , name          :: Name
  , nodeType      :: GT.NodeType
  , reload        :: T2.ReloadS
  , root          :: ID
  , session       :: Session
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  )

type IsLeaf = Boolean


nodeSpan :: R2.Leaf NodeSpanProps
nodeSpan = R2.leaf nodeSpanCpt
nodeSpanCpt :: R.Component NodeSpanProps
nodeSpanCpt = here.component "nodeSpan" cpt
  where
    cpt props@{ boxes: boxes@{ errors
                             , handed
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
        name' _ GT.NodeUser session = show session
        name' n _           _       = n

        isSelected = Just route' == Routes.nodeTypeAppRoute nodeType (sessionId session) id

        SettingsBox {show: showBox} = settingsBox nodeType

        href = url frontends $ GT.NodePath (sessionId session) nodeType (Just id)

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
        onDragOverHandler isDragOver e = do
          -- prevent redirection when file is dropped
          -- https://stackoverflow.com/a/6756680/941471
          SE.preventDefault e
          SE.stopPropagation e
          T.write_ true isDragOver

        onDragLeave :: forall event.
             T.Box Boolean
          -> SE.SyntheticEvent_ event
          -> Effect Unit
        onDragLeave isDragOver _ = T.write_ false isDragOver

        onTaskFinish ::
             GT.NodeID
          -> GT.AsyncTaskWithType
          -> Unit
          -> Effect Unit
        onTaskFinish id' t _ = do
          GAT.finish id' t tasks
          if GAT.asyncTaskTTriggersAppReload t then do
            here.log2 "reloading root for task" t
            T2.reload reloadRoot
          else do
            if GAT.asyncTaskTTriggersTreeReload t then do
              here.log2 "reloading tree for task" t
              T2.reload reload
            else do
              here.log2 "task doesn't trigger a tree reload" t
              pure unit
            if GAT.asyncTaskTTriggersMainPageReload t then do
              here.log2 "reloading main page for task" t
              T2.reload reloadMainPage
            else do
              here.log2 "task doesn't trigger a main page reload" t
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

      R.useEffect' $ do
        R.setRef setPopoverRef $ Just $ Popover.setOpen popoverRef

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
          folderIcon
          { isLeaf
          , isOpened: folderOpen'
          , callback: const $ T.modify_ (not) folderOpen
          }
        ,
          nodeIcon
          { nodeType
          , isLeaf
          , callback: const $ T.modify_ (not) folderOpen
          , id
          , name: name' props.name nodeType session
          }
        ,
          nodeLink
          { callback: onNodeLinkClick
          , href
          , id
          , name: name' props.name nodeType session
          , nodeType
          }
        ,
          fileTypeView
          { dispatch, droppedFile, id, isDragOver, nodeType } []
        ,
          R2.if' (showBox) $

            Popover.popover
            { arrow: false
            , open: false
            , onClose: \_ -> pure unit
            , onOpen:  \_ -> pure unit
            , ref: popoverRef
            }
            [
              H.div
              { className: "mainleaf__settings-icon" }
              [
                B.iconButton
                { name: "cog"
                -- (cf. Popover callbacks)
                , callback: const R.nothing
                , title:
                      "Each node of the Tree can perform some actions.\n"
                    <> "Click here to execute one of them."
                }
              ]
            ,
              nodePopupView
              { boxes
              , dispatch
              , id
              , name: name' props.name nodeType session
              , nodeType
              , onPopoverClose: const $ onPopoverClose popoverRef
              , session
              }
            ]
        ,
          -- H.span
          -- { style: { width: "8em" } }
          -- (map (\t -> asyncProgressBar { asyncTask: t
          --                                 --, barType: Pie
          --                               , barType: Bar
          --                               , errors
          --                               , nodeId: id
          --                               , onFinish: onTaskFinish id t
          --                               , session } []
          --       ) currentTasks'
          -- )
        -- ,
          R2.if' (nodeType == GT.NodeUser) $

            GV.versionView { session } []

        ,
          nodeActions
          { id
          , nodeType
          , refresh: const $ dispatch RefreshTree
          , session
          } []
        ]


---------------------------------------------------------

-- type NodeIconProps =
--   ( nodeType ::  GT.NodeType
--   , callback :: Unit -> Effect Unit
--   , isLeaf   :: Boolean
--   )

-- nodeIcon :: R2.Leaf NodeIconProps
-- nodeIcon = R2.leaf nodeIconCpt
-- nodeIconCpt :: R.Component NodeIconProps
-- nodeIconCpt = here.component "nodeIcon" cpt where
--   cpt { nodeType
--       , callback
--       , isLeaf } _ = pure $

--     B.iconButton
--     { className: "mainleaf__node-icon"
--     , name: GT.getIcon nodeType true
--     , callback
--     , status: isLeaf ? Idled $ Enabled
--     }

type NodeIconProps =
  ( nodeType ::  GT.NodeType
  , callback :: Unit -> Effect Unit
  , isLeaf   :: Boolean
  , name     :: GT.Name
  , id       :: ID
  )

nodeIcon :: R2.Leaf NodeIconProps
nodeIcon = R2.leaf nodeIconCpt
nodeIconCpt :: R.Component NodeIconProps
nodeIconCpt = here.component "nodeIcon" cpt where
  cpt { nodeType
      , callback
      , isLeaf
      , name
      , id
      } _ = pure $

    R.fragment
    [
      B.iconButton
      { className: "mainleaf__node-icon"
      , name: GT.getIcon nodeType true
      , callback
      , status: isLeaf ? Idled $ Enabled
      }
    ,
      ReactTooltip.reactTooltip
      { effect: "float"
      , id: name <> "-" <> (tooltipId id)
      , type: "dark"
      }
      [
        R2.row
        [
          H.h4
          { className: GT.fldr nodeType true }
          [ H.text $ GT.prettyNodeType nodeType ]
        ]
      ,
        R2.row
        [
          H.span {} [ H.text $ name ]
        ]
      ]
    ]


-----------------------------------------------

type FolderIconProps =
  ( isOpened :: Boolean
  , callback :: Unit -> Effect Unit
  , isLeaf   :: Boolean
  )

folderIcon :: R2.Leaf FolderIconProps
folderIcon = R2.leaf folderIconCpt
folderIconCpt :: R.Component FolderIconProps
folderIconCpt = here.component "folderIcon" cpt where
  cpt { isLeaf: true } _ = pure $

    B.icon
    { className: "mainleaf__folder-icon mainleaf__folder-icon--leaf"
    , name: "caret-right"
    }

  cpt { callback, isOpened } _ = pure $

    B.iconButton
    { className: "mainleaf__folder-icon"
    , name: isOpened ? "caret-down" $ "caret-right"
    , callback
    }

-----------------------------------------------


-- START node link

type NodeLinkProps =
  ( callback   :: Unit -> Effect Unit
  , href       :: String
  , id         :: Int
  , name       :: GT.Name
  , nodeType   :: GT.NodeType
  )

nodeLink :: R2.Leaf NodeLinkProps
nodeLink = R2.leaf nodeLinkCpt
nodeLinkCpt :: R.Component NodeLinkProps
nodeLinkCpt = here.component "nodeLink" cpt
  where
    cpt { callback
        , href
        , id
        , name
        , nodeType
        } _ = pure $

      H.div
      { className: "mainleaf__node-link"
      , on: { click: const $ callback unit }
      }
      [
        H.a
        { href
        , data:
            { for: name <> "-" <> (tooltipId id)
            , tip: true
            }
        }
        [
          B.span_ $ textEllipsisBreak 15 name
        -- ,
          -- ReactTooltip.reactTooltip
          -- { effect: "float"
          -- , id: name <> "-" <> (tooltipId id)
          -- , type: "dark"
          -- }
          -- [
          --   R2.row
          --   [
          --     H.h4
          --     { className: GT.fldr nodeType true }
          --     [ H.text $ GT.prettyNodeType nodeType ]
          --   ]
          -- ,
          --   R2.row
          --   [
          --     H.span {} [ H.text $ name ]
          --   ]
          -- ]
        ]
      ]

-- END node link

tooltipId :: GT.NodeID -> String
tooltipId id = "node-link-" <> show id


-----------------------------------------------

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
  cpt props _ = pure (child props.nodeType)
    where
      nodeActionsP      = SProxy :: SProxy "nodeType"

      childProps        = Record.delete nodeActionsP props

      child GT.NodeList = listNodeActions childProps
      child GT.Graph    = graphNodeActions childProps
      child _           = mempty

graphNodeActions :: R2.Leaf NodeActionsCommon
graphNodeActions = R2.leafComponent graphNodeActionsCpt
graphNodeActionsCpt :: R.Component NodeActionsCommon
graphNodeActionsCpt = here.component "graphNodeActions" cpt where
  cpt { id, session, refresh } _ =
    useLoader { errorHandler
              , loader: graphVersions session
              , path: id
              , render: \gv -> nodeActionsGraph { graphVersions: gv, session, id, refresh } [] }
  graphVersions session graphId = GraphAPI.graphVersions { graphId, session }
  errorHandler = logRESTError here "[graphNodeActions]"


listNodeActions :: R2.Leaf NodeActionsCommon
listNodeActions = R2.leafComponent listNodeActionsCpt
listNodeActionsCpt :: R.Component NodeActionsCommon
listNodeActionsCpt = here.component "listNodeActions" cpt where
  cpt { id, session, refresh } _ =
    useLoader { errorHandler
              , path: { nodeId: id, session }
              , loader: loadCorpusWithChild
              , render: \{ corpusId } -> nodeActionsNodeList
                 { listId: id, nodeId: corpusId, session, refresh: refresh
                 , nodeType: GT.TabNgramType GT.CTabTerms } }
    where
      errorHandler = logRESTError here "[listNodeActions]"
