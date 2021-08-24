module Gargantext.Components.Nodes.Corpus where

import DOM.Simple.Console (log, log2)
import Data.Array (snoc)
import Data.Array as A
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (null)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.CodeEditor as CE
import Gargantext.Components.FolderView as FV
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata)
import Gargantext.Components.Nodes.Types (FTField, FTFieldWithIndex, FTFieldsWithIndex(..), Field(..), FieldType(..), Hash, Index, defaultHaskell', defaultJSON', defaultMarkdown', defaultPython')
import Gargantext.Components.Tile (tileContext)
import Gargantext.Data.Array as GDA
import Gargantext.Hooks.LinkHandler (useLinkHandler)
import Gargantext.Prelude (Unit, bind, discard, pure, show, unit, ($), (<>), const, (<<<), (+), (==), (-), (<), (>), (<$>))
import Gargantext.Routes (SessionRoute(Children, NodeAPI), Tile)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (NodeType(..), AffTableResult)
import Gargantext.Utils.Crypto as Crypto
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus"

type Props =
  ( nodeId          :: Int
  , session         :: Session
  , tasks           :: T.Box GAT.Storage
  , reloadForest    :: T2.ReloadS
  , tileAxisXList   :: T.Box (Array (Record Tile))
  , tileAxisYList   :: T.Box (Array (Record Tile))
  )

corpusLayout :: R2.Leaf Props
corpusLayout props = R.createElement corpusLayoutCpt props []

corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = here.component "corpusLayout" cpt where
  cpt { nodeId
      , session
      , tasks
      , reloadForest
      , tileAxisXList
      , tileAxisYList
      } _ = do
    pure $ corpusLayoutMain { key
                            , nodeId
                            , session
                            , tasks
                            , reloadForest
                            , tileAxisXList
                            , tileAxisYList
                            }
      where
        key = show (sessionId session) <> "-" <> show nodeId

type KeyProps =
  ( nodeId          :: Int
  , key             :: String
  , session         :: Session
  , tasks           :: T.Box GAT.Storage
  , reloadForest    :: T2.ReloadS
  , tileAxisXList   :: T.Box (Array (Record Tile))
  , tileAxisYList   :: T.Box (Array (Record Tile))
  )

corpusLayoutMain :: R2.Leaf KeyProps
corpusLayoutMain props = R.createElement corpusLayoutMainCpt props []

corpusLayoutMainCpt :: R.Component KeyProps
corpusLayoutMainCpt = here.component "corpusLayoutMain" cpt
  where
    cpt props@{ nodeId, session, tasks, reloadForest } _ = do
      -- States
      popoverRef <- R.useRef null

      -- @WIP
      { goToRoute } <- useLinkHandler
      foo <- pure $ const $ goToRoute $ GR.CorpusCode (sessionId session) nodeId


      -- @addXTileCallback: open Code Corpus View into a new horizontal tile
      addXTileCallback <- pure $ const do
        id <- UUID.genUUID
        -- @WIP
        -- newTile <- pure { id, route: GR.CorpusCode (sessionId session) nodeId }
        newTile <- pure { id, route: GR.Corpus (sessionId session) nodeId }
        T.modify_ (\arr -> snoc arr newTile) props.tileAxisXList
        Popover.setOpen popoverRef false
      -- @addYTileCallback: open Code Corpus View into a new vertical tile
      addYTileCallback <- pure $ const do
        id <- UUID.genUUID
        newTile <- pure { id, route: GR.CorpusCode (sessionId session) nodeId }
        T.modify_ (\arr -> snoc arr newTile) props.tileAxisYList
        Popover.setOpen popoverRef false


      pure $

        H.div
        {}
        [
          Popover.popover
          { arrow: false
          , open: false
          , onClose: const $ pure unit
          , onOpen: const $ pure unit
          , ref: popoverRef
          }
          [
            H.button
            { className: "btn btn-primary" }
            [
              H.i { className: "fa fa-code" } []
            ]
          ,
            H.div { className: "popover-content" }
            [
              H.div { className: "card" }
              [
                H.div { className: "list-group" }
                [
                  H.ul {}
                  [
                    -- Add vertical tile
                    H.li {}
                    [
                      H.button
                      { className: "btn btn-link"
                      , on: { click: addYTileCallback }
                      }
                      [
                        H.i { className: "fa fa-angle-double-right mr-2" } []
                      ,
                        H.text "open on new tile"
                      ]
                    ]
                  ,
                    -- Add horizontal tile
                    H.li {}
                    [
                      H.button
                      { className: "btn btn-link"
                      , on: { click: addXTileCallback }
                      }
                      [
                        H.i { className: "fa fa-angle-double-down mr-2" } []
                      ,
                        H.text "open on new tile"
                      ]
                    ]

                  ,
                    -- @WIP
                    H.li {}
                    [
                      H.button
                      { className: "btn btn-link"
                      , on: { click: foo }
                      }
                      [
                        H.text "foo"
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]

        ,
          FV.folderView
          { nodeId
          , session
          , backFolder: true
          , tasks
          , reloadForest
          }
        ]



loadCorpusWithChild :: Record LoadProps -> Aff CorpusData
loadCorpusWithChild { nodeId: childId, session } = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session $ listNodeRoute childId ""
  corpusNode     <-  get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId)
                    :: forall a. JSON.ReadForeign a => AffTableResult (NodePoly a)
  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure { corpusId, corpusNode, defaultListId }
    Nothing ->
      throwError $ error "Missing default list"
  where
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    listNodeRoute       = NodeAPI Node <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just

-----------------------------------


type FieldsCodeEditorProps =
  (
    fields :: T.Box FTFieldsWithIndex
    | LoadProps
  )

fieldsCodeEditor :: R2.Component FieldsCodeEditorProps
fieldsCodeEditor = R.createElement fieldsCodeEditorCpt
fieldsCodeEditorCpt :: R.Component FieldsCodeEditorProps
fieldsCodeEditorCpt = here.component "fieldsCodeEditorCpt" cpt
  where
    cpt { fields, nodeId, session } _ = do
      (FTFieldsWithIndex fields') <- T.useLive T.unequal fields
      masterKey <- T.useBox T2.newReload
      masterKey' <- T.useLive T.unequal masterKey

      let editorsMap { idx, ftField } =
            fieldCodeEditorWrapper { canMoveDown: idx < (List.length fields' - 1)
                                   , canMoveUp: idx > 0
                                   , field: ftField
                                   , key: (show masterKey') <> "-" <> (show idx)
                                   , onChange: onChange idx
                                   , onMoveDown: onMoveDown masterKey idx
                                   , onMoveUp: onMoveUp masterKey idx
                                   , onRemove: onRemove idx
                                   , onRename: onRename idx
                                   }

      pure $ H.div {} $ List.toUnfoldable (editorsMap <$> fields')
      where
        onChange :: Index -> FieldType -> Effect Unit
        onChange idx typ = do
          T.modify_ (\(FTFieldsWithIndex fs) ->
            FTFieldsWithIndex $ fromMaybe fs $
              List.modifyAt idx (\{ ftField: Field f} -> { idx, ftField: Field $ f { typ = typ } }) fs) fields

        onMoveDown :: T2.ReloadS -> Index -> Unit -> Effect Unit
        onMoveDown masterKey idx _ = do
          T2.reload masterKey
          T.modify_ (\(FTFieldsWithIndex fs) -> recomputeIndices $ FTFieldsWithIndex $ GDA.swapList idx (idx + 1) fs) fields

        onMoveUp :: T2.ReloadS -> Index -> Unit -> Effect Unit
        onMoveUp masterKey idx _ = do
          T2.reload masterKey
          T.modify_ (\(FTFieldsWithIndex fs) -> recomputeIndices $ FTFieldsWithIndex $ GDA.swapList idx (idx - 1) fs) fields

        onRemove :: Index -> Unit -> Effect Unit
        onRemove idx _ = do
          T.modify_ (\(FTFieldsWithIndex fs) -> FTFieldsWithIndex $ fromMaybe fs $ List.deleteAt idx fs) fields

        onRename :: Index -> String -> Effect Unit
        onRename idx newName = do
          T.modify_ (\(FTFieldsWithIndex fs) ->
            FTFieldsWithIndex $ fromMaybe fs $
              List.modifyAt idx (\{ ftField: Field f } -> { idx, ftField: Field $ f { name = newName } }) fs) fields

    recomputeIndices :: FTFieldsWithIndex -> FTFieldsWithIndex
    recomputeIndices (FTFieldsWithIndex lst) = FTFieldsWithIndex $ List.mapWithIndex (\idx -> \{ ftField } -> { idx, ftField }) lst

hash :: FTFieldWithIndex -> Hash
hash { idx, ftField } = Crypto.hash $ "--idx--" <> (show idx) <> "--field--" <> (show ftField)

type FieldCodeEditorProps =
  (
    canMoveDown :: Boolean
  , canMoveUp   :: Boolean
  , field       :: FTField
  , key         :: String
  , onChange    :: FieldType -> Effect Unit
  , onMoveDown  :: Unit -> Effect Unit
  , onMoveUp    :: Unit -> Effect Unit
  , onRemove    :: Unit -> Effect Unit
  , onRename    :: String -> Effect Unit
  )

fieldCodeEditorWrapper :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditorWrapper props = R.createElement fieldCodeEditorWrapperCpt props []

fieldCodeEditorWrapperCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorWrapperCpt = here.component "fieldCodeEditorWrapperCpt" cpt
  where
    cpt props@{canMoveDown, canMoveUp, field: Field { name }, onMoveDown, onMoveUp, onRemove, onRename} _ = do
      pure $ H.div { className: "card mb-3" } [
        H.div { className: "card-header" } [
          H.div { className: "code-editor-heading row no-gutters justify-content-between" } [
              H.div { className: "col-5" } [
                 inputWithEnter { onBlur: onRename
                                , onEnter: \_ -> pure unit
                                , onValueChanged: onRename
                                , autoFocus: false
                                , className: "form-control"
                                , defaultValue: name
                                , placeholder: "Enter file name"
                                , type: "text" }
              ]
            , H.div { className: "d-flex flex-column" } ([
                H.div { className: "btn btn-danger mb-1"
                      , on: { click: \_ -> onRemove unit }
                      } [
                  H.span { className: "fa fa-trash" } [  ]
                  ]
              ] <> moveButtons)
            ]
         ]
        , H.div { className: "card-body" } [
           fieldCodeEditor props
           ]
        ]
      where
        moveButtons = [] <> (if canMoveDown then [moveDownButton] else [])
                         <> (if canMoveUp then [moveUpButton] else [])
        moveDownButton =
          H.div { className: "btn btn-primary"
                , on: { click: \_ -> onMoveDown unit }
                } [
            H.span { className: "fa fa-arrow-down" } [  ]
            ]
        moveUpButton =
          H.div { className: "btn btn-primary"
                , on: { click: \_ -> onMoveUp unit }
                } [
            H.span { className: "fa fa-arrow-up" } [  ]
            ]

type RenameableProps =
  (
    onRename :: String -> Effect Unit
  , text :: String
  )

renameable :: Record RenameableProps -> R.Element
renameable props = R.createElement renameableCpt props []

renameableCpt :: R.Component RenameableProps
renameableCpt = here.component "renameableCpt" cpt
  where
    cpt {onRename, text} _ = do
      isEditing <- T.useBox false
      state <- T.useBox text
      textRef <- R.useRef text

      -- handle props change of text
      R.useEffect1' text $ do
        if R.readRef textRef == text then
          pure unit
        else do
          R.setRef textRef text
          T.write_ text state

      pure $ H.div { className: "renameable" } [
        renameableText { isEditing, onRename, state }
      ]

type RenameableTextProps =
  (
    isEditing :: T.Box Boolean
  , onRename  :: String -> Effect Unit
  , state     :: T.Box String
  )

renameableText :: Record RenameableTextProps -> R.Element
renameableText props = R.createElement renameableTextCpt props []

renameableTextCpt :: R.Component RenameableTextProps
renameableTextCpt = here.component "renameableTextCpt" cpt
  where
    cpt { isEditing, onRename, state } _ = do
      isEditing' <- T.useLive T.unequal isEditing
      state' <- T.useLive T.unequal state

      pure $ if isEditing' then
              H.div { className: "input-group" }
                [ inputWithEnter {
                    autoFocus: false
                  , className: "form-control text"
                  , defaultValue: state'
                  , onBlur: \st -> T.write_ st state
                  , onEnter: submit state'
                  , onValueChanged: \st -> T.write_ st state
                  , placeholder: ""
                  , type: "text"
                  }
                , H.div { className: "btn input-group-append"
                        , on: { click: submit state' } }
                  [ H.span { className: "fa fa-floppy-o" } []
                  ]
                ]
             else
               H.div { className: "input-group" }
               [ H.input { className: "form-control"
                         , defaultValue: state'
                         , disabled: 1
                         , type: "text" }
               , H.div { className: "btn input-group-append"
                       , on: { click: \_ -> T.write_ true isEditing } }
                 [ H.span { className: "fa fa-pencil" } []
                 ]
               ]
      where
        submit text _ = do
          T.write_ false isEditing
          onRename text

fieldCodeEditor :: Record FieldCodeEditorProps -> R.Element
fieldCodeEditor props = R.createElement fieldCodeEditorCpt props []

fieldCodeEditorCpt :: R.Component FieldCodeEditorProps
fieldCodeEditorCpt = here.component "fieldCodeEditorCpt" cpt
  where
    cpt {field: Field {typ: typ@(Haskell {haskell})}, onChange} _ = do
      pure $ CE.codeEditor {code: haskell, defaultCodeType: CE.Haskell, onChange: changeCode onChange typ}

    cpt {field: Field {typ: typ@(Python {python})}, onChange} _ = do
      pure $ CE.codeEditor {code: python, defaultCodeType: CE.Python, onChange: changeCode onChange typ}

    cpt {field: Field {typ: typ@(JSON j)}, onChange} _ = do
      pure $ CE.codeEditor {code, defaultCodeType: CE.JSON, onChange: changeCode onChange typ}
      where
        code = R2.stringify (JSON.writeImpl j) 2

    cpt {field: Field {typ: typ@(Markdown {text})}, onChange} _ = do
      pure $ CE.codeEditor {code: text, defaultCodeType: CE.Markdown, onChange: changeCode onChange typ}

-- Performs the matrix of code type changes
-- (FieldType -> Effect Unit) is the callback function for fields array
-- FieldType is the current element that we will modify
-- CE.CodeType is the editor code type (might have been the cause of the trigger)
-- CE.Code is the editor code (might have been the cause of the trigger)
changeCode :: (FieldType -> Effect Unit) -> FieldType -> CE.CodeType -> CE.Code -> Effect Unit
changeCode onc (Haskell hs)        CE.Haskell  c = onc $ Haskell $ hs { haskell = c }
changeCode onc (Haskell hs)        CE.Python   c = onc $ Python   $ defaultPython'   { python  = c }
changeCode onc (Haskell {haskell}) CE.JSON     c = onc $ JSON     $ defaultJSON'     { desc = haskell }
changeCode onc (Haskell {haskell}) CE.Markdown c = onc $ Markdown $ defaultMarkdown' { text = haskell }

changeCode onc (Python hs)       CE.Python   c = onc $ Python  $ hs { python  = c }
changeCode onc (Python hs)       CE.Haskell  c = onc $ Haskell $ defaultHaskell' { haskell = c }
changeCode onc (Python {python}) CE.JSON     c = onc $ JSON     $ defaultJSON' { desc = python }
changeCode onc (Python {python}) CE.Markdown c = onc $ Markdown $ defaultMarkdown' { text = python }

changeCode onc (Markdown md) CE.Haskell  c = onc $ Haskell  $ defaultHaskell'  { haskell = c }
changeCode onc (Markdown md) CE.Python   c = onc $ Python   $ defaultPython'   { python  = c }
changeCode onc (Markdown md) CE.JSON     c = onc $ Markdown $ defaultMarkdown' { text    = c }
changeCode onc (Markdown md) CE.Markdown c = onc $ Markdown $ md               { text    = c }

changeCode onc (JSON j@{desc}) CE.Haskell c = onc $ Haskell $ defaultHaskell' { haskell = haskell }
  where
    haskell = R2.stringify (JSON.writeImpl j) 2
changeCode onc (JSON j@{desc}) CE.Python c = onc $ Python $ defaultPython' { python = toCode }
  where
    toCode = R2.stringify (JSON.writeImpl j) 2
changeCode onc _ CE.JSON c = do
  case JSON.readJSON c of
    Left err -> log2 "[fieldCodeEditor'] cannot parse json" c
    Right j' -> onc $ JSON j'
  -- case jsonParser c of
  --   Left err -> log2 "[fieldCodeEditor'] cannot parse json" c
  --   Right j' -> case decodeJson j' of
  --     Left err -> log2 "[fieldCodeEditor'] cannot decode json" j'
  --     Right j'' -> onc $ JSON j''
changeCode onc (JSON j) CE.Markdown _ = onc $ Markdown $ defaultMarkdown' { text = text }
  where
    text = R2.stringify (JSON.writeImpl j) 2


type LoadProps =
  ( nodeId  :: Int
  , session :: Session
  )

loadCorpus' :: Record LoadProps -> Aff (NodePoly Hyperdata)
loadCorpus' {nodeId, session} = get session $ NodeAPI Corpus (Just nodeId) ""

-- Just to make reloading effective
loadCorpusWithReload :: { reload :: T2.Reload  | LoadProps } -> Aff (NodePoly Hyperdata)
loadCorpusWithReload {nodeId, session} = loadCorpus' {nodeId, session}

type SaveProps = (
  hyperdata :: Hyperdata
  | LoadProps
  )

saveCorpus :: Record SaveProps -> Aff Unit
saveCorpus {hyperdata, nodeId, session} = do
  _id <- (put session (NodeAPI Corpus (Just nodeId) "") hyperdata) :: Aff Int
  pure unit

loadCorpus :: Record LoadProps -> Aff CorpusData
loadCorpus {nodeId, session} = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session nodePolyRoute
  corpusNode     <-  get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId)
                    :: forall a. JSON.ReadForeign a => AffTableResult (NodePoly a)
  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyRoute       = NodeAPI Corpus (Just nodeId) ""
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just

type LoadWithReloadProps =
  (
    reload :: T2.Reload
  | LoadProps
  )


-- Just to make reloading effective
loadCorpusWithChildAndReload :: Record LoadWithReloadProps -> Aff CorpusData
loadCorpusWithChildAndReload {nodeId, session} = loadCorpusWithChild {nodeId, session}
