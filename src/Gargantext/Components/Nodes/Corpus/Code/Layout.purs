module Gargantext.Components.Nodes.Corpus.Code where

import Data.List as List
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (fieldsCodeEditor, loadCorpusWithReload, saveCorpus)
import Gargantext.Components.Nodes.Corpus.Types (Hyperdata(..))
import Gargantext.Components.Nodes.Types (FTFieldList(..), FTFieldsWithIndex(..), defaultField)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, discard, pure, unit, ($), (<$>), (<>), (==))
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Code"

type Props =
  ( nodeId          :: Int
  , session         :: Session
  , reloadForest    :: T2.ReloadS
  )

type ViewProps =
  ( corpus  :: NodePoly Hyperdata
  , nodeId  :: Int
  , reload  :: T2.ReloadS
  , session :: Session
  )

corpusCodeLayout :: R2.Leaf Props
corpusCodeLayout props = R.createElement corpusCodeLayoutCpt props []
corpusCodeLayoutCpt :: R.Component Props
corpusCodeLayoutCpt = here.component "corpusCodeLayout" cpt where
  cpt { nodeId, session } _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { nodeId, reload: reload', session } loadCorpusWithReload $
      \corpus -> corpusCodeView { corpus, nodeId, reload, session }

corpusCodeView :: Record ViewProps -> R.Element
corpusCodeView props = R.createElement corpusCodeViewCpt props []
corpusCodeViewCpt :: R.Component ViewProps
corpusCodeViewCpt = here.component "corpusCodeView" cpt where
  cpt {corpus: (NodePoly {hyperdata: Hyperdata {fields: FTFieldList fields}}), nodeId, reload, session} _ = do
    let fieldsWithIndex = FTFieldsWithIndex $ List.mapWithIndex (\idx -> \ftField -> { idx, ftField }) fields
    fieldsS <- T.useBox fieldsWithIndex
    fields' <- T.useLive T.unequal fieldsS
    fieldsRef <- R.useRef fields

    -- handle props change of fields
    R.useEffect1' fields $ do
      if R.readRef fieldsRef == fields then
        pure unit
      else do
        R.setRef fieldsRef fields
        T.write_ fieldsWithIndex fieldsS

    pure $

      H.div
      {}
      [
        H.div
        { className: "mb-4" }
        [
          H.div
          { className: "btn btn-primary " <> (saveEnabled fieldsWithIndex fields')
          , on: { click: onClickSave {fields: fields', nodeId, reload, session} }
          }
          [ H.span { className: "fa fa-floppy-o" } [ ] ]
        ]
      ,
        H.div
        {}
        [
          fieldsCodeEditor
          { fields: fieldsS
          , nodeId
          , session }
          []
        ]
      ,
        H.div
        { className: "mb-4" }
        [
          H.div
          { className: "btn btn-primary"
          , on: { click: onClickAdd fieldsS }
          }
          [ H.span { className: "fa fa-plus" } [  ] ]
        ]
      ]

  saveEnabled :: FTFieldsWithIndex -> FTFieldsWithIndex -> String
  saveEnabled fs fsS = if fs == fsS then "disabled" else "enabled"

  onClickSave :: forall e. { fields :: FTFieldsWithIndex
                          , nodeId :: Int
                          , reload :: T2.ReloadS
                          , session :: Session } -> e -> Effect Unit
  onClickSave {fields: FTFieldsWithIndex fields, nodeId, reload, session} _ = do
    launchAff_ do
      saveCorpus $ { hyperdata: Hyperdata {fields: FTFieldList $ (_.ftField) <$> fields}
                  , nodeId
                  , session }
      liftEffect $ T2.reload reload

  onClickAdd :: forall e. T.Box FTFieldsWithIndex -> e -> Effect Unit
  onClickAdd fieldsS _ = do
    T.modify_ (\(FTFieldsWithIndex fs) -> FTFieldsWithIndex $
      List.snoc fs $ { idx: List.length fs, ftField: defaultField }) fieldsS
