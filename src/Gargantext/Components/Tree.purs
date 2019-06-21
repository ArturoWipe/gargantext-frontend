module Gargantext.Components.Tree where

import Prelude hiding (div)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Cont.Trans (lift)
import Data.Array (filter)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import FFI.Simple ((..))
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement)
import React as React
import React.DOM (a, button, div, h5, i, input, li, span, text, ul, b, u, select, option)
import React.DOM.Props (_id, _type, className, href, title, onClick, onDrop, onDragOver, onInput, placeholder, style, defaultValue, _data)
import React.DOM.Props as DOM
import React.SyntheticEvent as E
import Thermite (PerformAction, Render, Spec, createClass, defaultPerformAction, simpleSpec, modifyState_)
import URI.Extra.QueryPairs as QP
import URI.Query as Q
import Web.File.File (toBlob)
import Web.File.FileReader.Aff (readAsText)
import Web.File.FileList (FileList, item)

import Gargantext.Config (toUrl, End(..), NodeType(..), urlPlease)
import Gargantext.Config.REST (get, put, post, postWwwUrlencoded, delete)
import Gargantext.Components.Loader as Loader
import Gargantext.Types (class ToQuery, toQuery)
import Gargantext.Utils (id)

type Name = String
type Open = Boolean
type URL  = String
type ID   = Int

type Props = { root :: ID }

data NTree a = NTree a (Array (NTree a))

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

-- Keep only the nodes matching the predicate.
-- The root of the tree is always kept.
filterNTree :: forall a. (a -> Boolean) -> NTree a -> NTree a
filterNTree p (NTree x ary) =
  NTree x $ map (filterNTree p) $ filter (\(NTree a _) -> p a) ary

type FTree = NTree LNode

-- file upload types
data FileType = CSV | PresseRIS
derive instance genericFileType :: Generic FileType _
instance eqFileType :: Eq FileType where
    eq = genericEq
instance showFileType :: Show FileType where
    show = genericShow
newtype UploadFileContents = UploadFileContents String
data DroppedFile = DroppedFile {
    contents :: UploadFileContents
  , fileType :: Maybe FileType
    }
type FileHash = String


data Action =   ShowPopOver  ID
              | ToggleFolder ID
              | RenameNode   String ID
              | Submit       ID String
              | DeleteNode   ID
              | Create       ID
              | CreateSubmit       ID String
              | SetNodeValue String ID
              | ToggleCreateNode ID
              | ShowRenameBox    ID
              | CancelRename     ID
              | CurrentNode      ID
              | PrepareUploadFile ID UploadFileContents
              | UploadFile ID FileType UploadFileContents


type State = { state       :: FTree
             , currentNode :: Maybe ID
             }

-- TODO remove
initialNode :: { createNode :: Boolean
               , id :: ID
               , name :: String
               , nodeType :: NodeType
               , nodeValue :: String
               , open :: Boolean
               , popOver :: Boolean
               , renameNodeValue :: String
               , droppedFile :: Maybe DroppedFile
               , showRenameBox :: Boolean
               }
initialNode = { id : 3
              , name : "hello"
              , nodeType : Node
              , open : true
              , popOver : false
              , renameNodeValue : ""
              , createNode : false
              , nodeValue : "InitialNode"
              , droppedFile : Nothing
              , showRenameBox : false}

initialState :: State
initialState = { state: NTree (LNode initialNode) []
               , currentNode : Nothing}

mapFTree :: (FTree -> FTree) -> State -> State
mapFTree f {state, currentNode} = {state: f state, currentNode: currentNode}

-- TODO: make it a local function
performAction :: forall props. PerformAction State props Action

performAction (ToggleFolder i) _ _ =
  modifyState_ $ mapFTree $ toggleNode i

performAction (ShowPopOver id) _ _ =
  modifyState_ $ mapFTree $ map $ popOverNode id

performAction (ShowRenameBox id) _ _ =
  modifyState_ $ mapFTree $ map $ showPopOverNode id

performAction (CancelRename id) _ _ =
  modifyState_ $ mapFTree $ map $ showPopOverNode id

performAction (ToggleCreateNode id) _ _ = do
  modifyState_ $ mapFTree $ map $ hidePopOverNode id
  modifyState_ $ mapFTree $ showCreateNode id

performAction (DeleteNode nid) _ _ = do
  void $ lift $ deleteNode nid
  modifyState_ $ mapFTree $ filterNTree (\(LNode {id}) -> id /= nid)

performAction (Submit rid name) _  _  = do
  void $ lift $ renameNode rid $ RenameValue {name}
  modifyState_ $ mapFTree $ map $ popOverNode rid
                              <<< onNode rid (\(LNode node) -> LNode (node { name = name }))

performAction (RenameNode  r nid) _ _ =
  modifyState_ $ mapFTree $ rename nid r

performAction (CreateSubmit nid name) _ _ = do
  void $ lift $ createNode $ CreateValue {name}
  modifyState_ $ mapFTree $ map $ hidePopOverNode nid

performAction (Create  nid) _ _ = do
  modifyState_ $ mapFTree $ showCreateNode nid

performAction (SetNodeValue v nid) _ _ =
  modifyState_ $ mapFTree $ setNodeValue nid v

performAction (CurrentNode nid) _ _ =
  modifyState_ $ \{state: s} -> {state: s, currentNode : Just nid}

performAction (PrepareUploadFile nid contents) _ _ = do
  modifyState_ $ mapFTree $ map $ toggleFileTypeBox nid contents

performAction (UploadFile nid fileType contents) _ _ = do
  hashes <- lift $ uploadFile nid fileType contents
  liftEffect $ log2 "uploaded:" hashes


toggleIf :: Boolean -> Boolean -> Boolean
toggleIf true  = not
toggleIf false = const false

onNode :: ID -> (LNode -> LNode) -> LNode -> LNode
onNode id f l@(LNode node)
  | node.id == id = f l
  | otherwise     = l

popOverNode :: ID -> LNode -> LNode
popOverNode sid (LNode node) =
  LNode $ node { popOver = toggleIf (sid == node.id) node.popOver
               , showRenameBox = false }

hidePopOverNode :: ID -> LNode -> LNode
hidePopOverNode sid (LNode node) =
  LNode $ node { popOver = false }

showPopOverNode :: ID -> LNode -> LNode
showPopOverNode sid (LNode node) =
  LNode $ node {showRenameBox = toggleIf (sid == node.id) node.showRenameBox}

toggleFileTypeBox :: ID -> UploadFileContents -> LNode -> LNode
toggleFileTypeBox sid contents (LNode node@{id, droppedFile: Nothing}) | sid == id = LNode $ node {droppedFile = droppedFile}
  where
    droppedFile = Just $ DroppedFile {contents: contents, fileType: Nothing}
toggleFileTypeBox sid _ (LNode node) = LNode $ node {droppedFile = Nothing}

-- TODO: DRY, NTree.map
showCreateNode :: ID -> NTree LNode -> NTree LNode
showCreateNode sid (NTree (LNode node@{id, createNode}) ary) =
  NTree (LNode $ node {createNode = createNode'}) $ map (showCreateNode sid) ary
  where
    createNode' = if sid == id then not createNode else createNode

----TODO get id and value to send API to call

-- getCreateNode :: ID -> NTree LNode -> String
-- getCreateNode sid (NTree (LNode {id, name, nodeType, open, popOver, renameNodeValue, createNode, nodeValue}) ary) =
--   createNode
--   where
--     NTree (LNode {id,name, nodeType, open , popOver, renameNodeValue, createNode , nodeValue}) $ map (getCreateNode sid) ary
--     createNode' = if sid == id then  nodeValue else ""

-- TODO: DRY, NTree.map
rename :: ID ->  String -> NTree LNode  -> NTree LNode
rename sid v (NTree (LNode node@{id}) ary)  =
  NTree (LNode $ node {renameNodeValue = rvalue}) $ map (rename sid  v) ary
  where
    rvalue = if sid == id then  v   else ""

-- TODO: DRY, NTree.map
setNodeValue :: ID ->  String -> NTree LNode  -> NTree LNode
setNodeValue sid v (NTree (LNode node@{id}) ary)  =
  NTree (LNode $ node {nodeValue = nvalue}) $ map (setNodeValue sid  v) ary
  where
    nvalue = if sid == id then  v   else ""

-- TODO: DRY, NTree.map
toggleNode :: ID -> NTree LNode -> NTree LNode
toggleNode sid (NTree (LNode node@{id, open}) ary) =
  NTree (LNode $ node {open = nopen}) $ map (toggleNode sid) ary
  where
    nopen = if sid == id then not open else open



------------------------------------------------------------------------
-- Realistic Tree for the UI

exampleTree :: NTree LNode
exampleTree = NTree (LNode { id : 1
                           , name : ""
                           , nodeType : Node
                           , open : false
                           , popOver : false
                           , renameNodeValue : ""
                           , createNode : false
                           , nodeValue : ""
                           , droppedFile: Nothing
                           , showRenameBox : false}) []

-- exampleTree :: NTree LNode
-- exampleTree =
--   NTree 1 true "françois.pineau"
--   [ --annuaire 2 "Annuaire"
--   --, corpus   3 "IMT publications"
--   ]

-- annuaire :: ID -> String -> NTree (Tuple String String)
-- annuaire n name = NTree n false name
--     [ NTree (Tuple "IMT community"    "#/docView")
--     ]

-- corpus :: ID -> String -> NTree (Tuple String String)
-- corpus n name = NTree (LNode {id : n, name, nodeType : "", open : false})
--     [ NTree (Tuple "Facets"    "#/corpus") []
--     , NTree (Tuple "Dashboard" "#/dashboard") []
--     , NTree (Tuple "Graph"     "#/graphExplorer") []
--     ]


------------------------------------------------------------------------
-- TODO
-- alignment to the right
nodeOptionsCorp :: Boolean -> Array ReactElement
nodeOptionsCorp activated = case activated of
                         true  -> [ i [className "fab fa-whmcs" ] []]
                         false -> []

-- TODO
-- alignment to the right
-- on hover make other options available:
nodeOptionsView :: Boolean -> Array ReactElement
nodeOptionsView activated = case activated of
                         true -> [ i [className "fas fa-sync-alt" ] []
                                 , i [className "fas fa-upload"   ] []
                                 , i [className "fas fa-share-alt"] []
                                 ]
                         false -> []


nodeOptionsRename :: (Action -> Effect Unit) ->  Boolean ->  ID -> Array ReactElement
nodeOptionsRename d activated  id =  case activated of
                         true -> [ a [className "glyphicon glyphicon-pencil", style {marginLeft : "15px"}

                                        ] []
                                 ]
                         false -> []

type LoadedTreeViewProps = Loader.InnerProps Int FTree ()

loadedTreeview :: Spec State LoadedTreeViewProps Action
loadedTreeview = simpleSpec performAction render
  where
    render :: Render State LoadedTreeViewProps Action
    render dispatch _ {state, currentNode} _ =
      [ div [className "tree"]
        [ toHtml dispatch state currentNode

        ]
      ]

treeViewClass :: ReactClass (Loader.InnerProps Int FTree (children :: React.Children))
treeViewClass = createClass "TreeView" loadedTreeview (\{loaded: t} -> {state: t, currentNode: Nothing})

treeLoaderClass :: Loader.LoaderClass Int FTree
treeLoaderClass = Loader.createLoaderClass "TreeLoader" loadNode

treeLoader :: Loader.Props' Int FTree -> ReactElement
treeLoader props = React.createElement treeLoaderClass props []

treeview :: Spec {} Props Void
treeview = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {root} _ _ =
      [ treeLoader { path: root
                   , component: treeViewClass
                   } ]

renameTreeView :: (Action -> Effect Unit) -> FTree -> ID -> ReactElement
renameTreeView d s@(NTree (LNode {id, name, renameNodeValue, popOver: true, showRenameBox }) ary) nid  =
  div [ className ""
      , _id "rename-tooltip"
      , _data {toggle: "tooltip", placement: "right"}
      , title "Settings on right"] $
  [ div [_id "arrow"] []
  , div [ className "panel panel-default"
        , style {border:"1px solid rgba(0,0,0,0.2)", boxShadow : "0 2px 5px rgba(0,0,0,0.2)"}]
    [ div [className "panel-heading"]
      [ div [ className "row" ] $
        [ div [className (if (showRenameBox) then "col-md-10" else "col-md-8")]
          [ if (showRenameBox) then renameBox else renameBoxLabel ]
        ] <> [ if (showRenameBox) then editIconDummy else editIcon ] <> [
          div [ className "col-md-2" ]
          [ a [className "btn text-danger glyphitem glyphicon glyphicon-remove"
              , onClick $ \_ -> d $ ShowPopOver nid
              , title "Close"] []
          ]
        ]
      ]
    , div [ className "panel-body"
          , style {display:"flex", justifyContent : "center", backgroundColor: "white", border: "none"}]
      [ div [className "col-md-4"]
        [a [ style iconAStyle
           , className (glyphicon "plus")
           , _id "rename1"
           , title "Create"
           , onClick $ (\_ -> d $ (ToggleCreateNode id))]
         []
        ]
      , div [className "col-md-4"]
        [a [ style iconAStyle
           , className (glyphicon "download-alt")
           , _id "rename1"
           , title "Download [WIP]"]
         []
        ]
      , div [className "col-md-4"]
        [a [ style iconAStyle
           , className (glyphicon "duplicate")
           , _id "rename1"
           , title "Duplicate [WIP]"]
         []
        ]
      , div [className "col-md-4"]
        [ a [ style iconAStyle
            , className (glyphicon "trash")
            , _id "rename2"
            , title "Delete"
            , onClick $ (\_-> d $ (DeleteNode id))]
          []
        ]
      ]
    ]
  ]
  where
    iconAStyle = {color:"black", paddingTop: "6px", paddingBottom: "6px"}
    glyphicon t = "glyphitem glyphicon glyphicon-" <> t
    editIcon = div [ className "col-md-2" ]
                [ a [ style {color:"black"}
                    , className "btn glyphitem glyphicon glyphicon-pencil"
                    , _id "rename1"
                    , title "Rename"
                    , onClick $ (\_-> d $ (ShowRenameBox id))]
                  []
                ]
    editIconDummy = div [] []
    renameBox = div [ className "from-group row-no-padding" ]
                [ div [className "col-md-8"]
                  [ input [ _type "text"
                          , placeholder "Rename Node"
                          , defaultValue $ name
                          , className "form-control"
                          , onInput \e -> d (RenameNode (unsafeEventValue e) nid)
                          ]
                  ]
                , a [className "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
                    , _type "button"
                    , onClick \_ -> d $ (Submit nid renameNodeValue)
                    , title "Rename"
                    ] []
                , a [className "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
                    , _type "button"
                    , onClick \_ -> d $ (CancelRename nid)
                    , title "Cancel"
                    ] []
                ]
    renameBoxLabel = div [] [ text name ]
renameTreeView _ _ _ = div [] []


createNodeView :: (Action -> Effect Unit) -> FTree -> ID -> ReactElement
createNodeView d s@(NTree (LNode { createNode: true, nodeValue }) ary) nid  =
  div [ className ""
      , _id "create-node-tooltip"
      , _data {toggle: "tooltip", placement: "right"}
      , title "Create new node"] $
  [ div [className "panel panel-default"]
    [ div [className "panel-heading"]
      [
        div [className "row"]
        [ div [ className "col-md-10"]
          [ h5 [] [text  "Create Node"] ]
        , div [className "col-md-2"]
          [ a [className "btn text-danger glyphitem glyphicon glyphicon-remove"
              , onClick $ \_ -> d $ ToggleCreateNode nid
              , title "Close"] []
          ]
        ]
      ]
    , div [className "panel-body"]
     [
       input [ _type "text"
             , placeholder "Create Node"
             , defaultValue $ getCreateNodeValue s
             , className "col-md-12 form-control"
             , onInput \e -> d (SetNodeValue (unsafeEventValue e) nid)
             ]
     ]
    , div [className "panel-footer"]
      [ button [className "btn btn-success"
               , _type "button"
               , onClick \_ -> d $ (CreateSubmit nid nodeValue)
               ] [text "Create"]
      ]
    ]
  ]
createNodeView _ _ _ = div [] []


fileTypeView :: (Action -> Effect Unit) -> FTree -> ID -> ReactElement
fileTypeView d s@(NTree (LNode {droppedFile: Just (DroppedFile {contents, fileType: Nothing})}) ary) nid =
  div [ className ""
      , _id "file-type-tooltip"
      , _data {toggle: "tooltip", placement: "right"}
      , title "Choose file type"] $
  [ div [className "panel panel-default"]
    [ div [className "panel-heading"]
      [
        div [className "row"]
        [ div [ className "col-md-10"]
          [ h5 [] [text  "Choose file type"] ]
        , div [className "col-md-2"]
          [ a [className "btn text-danger glyphitem glyphicon glyphicon-remove"
              , onClick $ \_ -> d $ PrepareUploadFile nid contents
              , title "Close"] []
          ]
        ]
      ]
    , div [className "panel-body"]
     [
       select [ className "col-md-12 form-control"
              --, onInput \e -> d (SetNodeValue (unsafeEventValue e) nid)
              ]
       (map renderOption [CSV, PresseRIS])
     ]
    , div [className "panel-footer"]
      [ button [className "btn btn-success"
               , _type "button"
               , onClick \_ -> d $ (UploadFile nid CSV contents)
               ] [text "Upload"]
      ]
    ]
  ]
  where
    renderOption opt = option [] [ text $ show opt ]
fileTypeView d s@(NTree (LNode node) ary) nid = div [] []

popOverValue :: FTree -> Boolean
popOverValue (NTree (LNode {popOver}) ary) = popOver

getCreateNodeValue :: FTree -> String
getCreateNodeValue (NTree (LNode {nodeValue}) ary) = nodeValue


toHtml :: (Action -> Effect Unit) -> FTree -> Maybe ID -> ReactElement
toHtml d s@(NTree (LNode {id, name, nodeType}) []) n =
  ul []
  [
    li [] $ [span []
    [ a [className "glyphicon glyphicon-cog", _id "rename-leaf",onClick $ (\_-> d $ (ShowPopOver id))] []
    , a [ href (toUrl Front nodeType (Just id)), style {"margin-left":"22px"}
        , onClick $ (\e -> d $ CurrentNode id)
        ]
      [ if n == (Just id) then u [] [b [] [text ("| " <> name <> " |    ")]] else text (name <> "    ") ]
    , renameTreeView d s id
    , createNodeView d s id
    , fileTypeView d s id
    ]
  ]]
--- need to add renameTreeview value to this function
toHtml d s@(NTree (LNode {id, name, nodeType, open}) ary) n =
    ul []
  [ li [] $
    ( [span [onDrop dropHandler, onDragOver onDragOverHandler] [
         a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
       , a [ href (toUrl Front nodeType (Just id)), style {"margin-left":"22px"}
           , onClick $ (\e -> d $ CurrentNode id)
           ]
         --[ text name ]
         [ if n == (Just id) then u [] [b [] [text $ "| " <> name <> " |"]] else text name ]
       , a [ className "glyphicon glyphicon-cog"
         , _id "rename"
         , onClick $ (\_-> d $ (ShowPopOver id))
         ] []
       , renameTreeView d s id
       , createNodeView d s id
       , fileTypeView d s id
       ]
      ] <> if open then
        map (\s -> toHtml d s n) ary
      else []
    )
  ]
  where
    dropHandler = \e -> unsafePartial $ do
      let ff = fromJust $ item 0 $ ((e .. "dataTransfer" .. "files") :: FileList)
      liftEffect $ log2 "drop:" ff
      -- prevent redirection when file is dropped
      E.preventDefault e
      E.stopPropagation e
      let blob = toBlob $ ff
      void $ runAff (\_ -> pure unit) do
        contents <- readAsText blob
        liftEffect $ d $ PrepareUploadFile id (UploadFileContents contents)
    onDragOverHandler = \e -> do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e


fldr :: Boolean -> DOM.Props
fldr open = if open then className "fas fa-folder-open" else className "fas fa-folder"


newtype LNode = LNode { id :: ID
                      , name :: String
                      , nodeType :: NodeType
                      , open :: Boolean
                      , popOver :: Boolean
                      , renameNodeValue :: String
                      , nodeValue :: String
                      , createNode :: Boolean
                      , droppedFile :: Maybe DroppedFile
                      , showRenameBox :: Boolean}

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .: "id"
    name <- obj .: "name"
    nodeType <- obj .: "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType
                 , open : true
                 , popOver : false
                 , renameNodeValue : ""
                 , createNode : false
                 , nodeValue : ""
                 , droppedFile: Nothing
                 , showRenameBox : false}

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .: "node"
    nodes <- obj .: "children"
    node' <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

loadNode :: ID -> Aff FTree
loadNode = get <<< toUrl Back Tree <<< Just

----- TREE CRUD Operations

newtype RenameValue = RenameValue
  {
    name :: String
  }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue {name})
     = "r_name" := name
    ~> jsonEmptyObject

newtype CreateValue = CreateValue
  {
    name :: String
  }

instance encodeJsonCreateValue :: EncodeJson CreateValue where
  encodeJson (CreateValue {name})
     = "query" := name
    ~> "corpus_id" := 0
    ~> "files_id" := ([] :: Array String)
    ~> jsonEmptyObject

createNode :: CreateValue -> Aff ID
createNode = post $ urlPlease Back $ "new"

renameNode :: ID -> RenameValue -> Aff (Array ID)
renameNode renameNodeId = put $ toUrl Back Node (Just renameNodeId) <> "/rename"

deleteNode :: ID -> Aff ID
deleteNode = delete <<< toUrl Back Node <<< Just

newtype FileUploadQuery = FileUploadQuery {
    fileType :: FileType
  }
derive instance newtypeSearchQuery :: Newtype FileUploadQuery _
instance fileUploadQueryToQuery :: ToQuery FileUploadQuery where
  toQuery (FileUploadQuery {fileType}) =
    QP.print id id $ QP.QueryPairs $
         pair "fileType" fileType
    where pair :: forall a. Show a => String -> a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k v = [ QP.keyFromString k /\ (Just $ QP.valueFromString $ show v) ]

uploadFile :: ID -> FileType -> UploadFileContents -> Aff (Array FileHash)
uploadFile id fileType (UploadFileContents fileContents) = postWwwUrlencoded url fileContents
  where
    q = FileUploadQuery { fileType: fileType }
    url = toUrl Back Node (Just id) <> "/upload" <> Q.print (toQuery q)

-- UNUSED
-- deleteNodes :: TODO -> Aff ID
-- deleteNodes = deleteWithBody (toUrl Back Nodes Nothing)

-- UNUSED
-- createNode :: TODO -> Aff ID
-- createNode = post (toUrl Back Node Nothing)

fnTransform :: LNode -> FTree
fnTransform n = NTree n []

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
