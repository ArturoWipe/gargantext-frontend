module Gargantext.Components.Forest.Tree.Node.Action where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Prelude (class Show, Unit)
import Gargantext.Sessions (Session)
import Gargantext.Types  as GT
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut, SubTreeParams(..))
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..), glyphiconNodeAction)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType, UploadFileContents)
import Gargantext.Components.Forest.Tree.Node.Action.Update.Types (UpdateNodeParams)


type Props =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , nodeType :: GT.NodeType
  , session  :: Session
  )


data Action = AddNode     String GT.NodeType
            | DeleteNode
            | RenameNode  String
            | UpdateNode  UpdateNodeParams
            | ShareNode   String
            | DoSearch    GT.AsyncTaskWithType
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileContents
            | DownloadNode
            | RefreshTree

            | MoveNode  {params :: Maybe SubTreeOut}
            | MergeNode {params :: Maybe SubTreeOut}
            | LinkNode  {params :: Maybe SubTreeOut}

            | NoAction


subTreeOut :: Action -> Maybe SubTreeOut
subTreeOut (MoveNode  {params}) = params
subTreeOut (MergeNode {params}) = params
subTreeOut (LinkNode  {params}) = params
subTreeOut _                    = Nothing

setTreeOut ::  Action -> Maybe SubTreeOut -> Action
setTreeOut (MoveNode  {params:_}) p = MoveNode  {params: p}
setTreeOut (MergeNode {params:_}) p = MergeNode {params: p}
setTreeOut (LinkNode  {params:_}) p = LinkNode  {params: p}
setTreeOut a   _             = a


instance showShow :: Show Action where
  show (AddNode     _ _    )= "AddNode"
  show  DeleteNode          = "DeleteNode"
  show (RenameNode  _      )= "RenameNode"
  show (UpdateNode  _      )= "UpdateNode"
  show (ShareNode   _      )= "ShareNode"
  show (DoSearch    _      )= "SearchQuery"
  show (UploadFile  _ _ _ _)= "UploadFile"
  show  RefreshTree         = "RefreshTree"
  show  DownloadNode        = "Download"
  show (MoveNode  _ )       = "MoveNode"
  show (MergeNode _ )      = "MergeNode"
  show (LinkNode  _ )       = "LinkNode"
  show NoAction             = "NoAction"

-----------------------------------------------------------------------
icon :: Action -> String
icon (AddNode    _ _)     = glyphiconNodeAction (Add [])
icon  DeleteNode          = glyphiconNodeAction Delete
icon (RenameNode _)       = glyphiconNodeAction Config
icon (UpdateNode _)       = glyphiconNodeAction Refresh
icon (ShareNode  _)       = glyphiconNodeAction Share
icon (DoSearch   _)       = glyphiconNodeAction SearchBox
icon (UploadFile _ _ _ _) = glyphiconNodeAction Upload
icon  RefreshTree         = glyphiconNodeAction Refresh
icon  DownloadNode        = glyphiconNodeAction Download
icon (MoveNode _ )        = glyphiconNodeAction (Move { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (MergeNode _ )       = glyphiconNodeAction (Merge { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (LinkNode _  )       = glyphiconNodeAction (Link { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})

icon NoAction             = "hand-o-right"

-- icon _             = "hand-o-right"

text :: Action -> String
text (AddNode     _ _    )= "Add !"
text  DeleteNode          = "Delete !"
text (RenameNode  _      )= "Rename !"
text (UpdateNode  _      )= "Update !"
text (ShareNode   _      )= "Share !"
text (DoSearch    _      )= "Launch search !"
text (UploadFile  _ _ _ _)= "Upload File !"
text  RefreshTree         = "Refresh Tree !"
text DownloadNode         = "Download !"
text (MoveNode  _ )      = "Move !"
text (MergeNode _ )     = "Merge !"
text (LinkNode  _ )      = "Link !"
text NoAction             = "No Action"
-----------------------------------------------------------------------
