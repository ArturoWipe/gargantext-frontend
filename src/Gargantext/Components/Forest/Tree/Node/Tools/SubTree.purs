module Gargantext.Components.Forest.Tree.Node.Tools.SubTree where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action (Props, Action, subTreeOut, setTreeOut)
import Gargantext.Components.Forest.Tree.Node.Tools (nodeText)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeParams(..), SubTreeOut(..))
import Gargantext.Config.REST (RESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), get)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Tools.SubTree"

type SubTreeParamsIn =
  ( boxes         :: Boxes
  , subTreeParams :: SubTreeParams
  | Props
  )

------------------------------------------------------------------------
type SubTreeParamsProps =
  ( action    :: T.Box Action
  | SubTreeParamsIn
  )

subTreeView :: R2.Component SubTreeParamsProps
subTreeView = R.createElement subTreeViewCpt
subTreeViewCpt :: R.Component SubTreeParamsProps
subTreeViewCpt = here.component "subTreeView" cpt
  where
    cpt { action
        , boxes
        , dispatch
        , id
        , nodeType
        , session
        , subTreeParams
        } _ = do
      let
        SubTreeParams {showtypes} = subTreeParams
      --  (valAction /\ setAction)  = action
      -- _ <- pure $ setAction (const $ setTreeOut valAction Nothing)

      useLoader { errorHandler
                , loader: loadSubTree showtypes
                , path: session
                , render: \tree ->
                    subTreeViewLoaded { action
                                      , boxes
                                      , dispatch
                                      , id
                                      , nodeType
                                      , session
                                      , subTreeParams
                                      , tree
                                      } []  }
      where
        errorHandler err = here.log2 "RESTError" err

loadSubTree :: Array GT.NodeType -> Session -> Aff (Either RESTError FTree)
loadSubTree nodetypes session = getSubTree session treeId nodetypes
  where
    Session { treeId } = session

getSubTree :: Session -> Int -> Array GT.NodeType -> Aff (Either RESTError FTree)
getSubTree session treeId showtypes = get session $ GR.NodeAPI GT.Tree (Just treeId) nodeTypes
  where
    nodeTypes     = A.foldl (\a b -> a <> "type=" <> show b <> "&") "?" showtypes

------------------------------------------------------------------------
type CorpusTreeProps =
  ( tree         :: FTree
  | SubTreeParamsProps
  )

subTreeViewLoaded :: R2.Component CorpusTreeProps
subTreeViewLoaded = R.createElement subTreeViewLoadedCpt
subTreeViewLoadedCpt :: R.Component CorpusTreeProps
subTreeViewLoadedCpt = here.component "subTreeViewLoaded" cpt where
  cpt props _ = do

    let pRender = Record.merge { render: subTreeTreeView } props

    pure $

      H.div { className: "subtree" }
      [ subTreeTreeView (CorpusTreeRenderProps pRender) [] ]

newtype CorpusTreeRenderProps = CorpusTreeRenderProps
  { render :: CorpusTreeRenderProps -> Array R.Element -> R.Element
  | CorpusTreeProps
  }

subTreeTreeView :: CorpusTreeRenderProps -> Array R.Element -> R.Element
subTreeTreeView = R2.ntCreateElement subTreeTreeViewCpt
subTreeTreeViewCpt :: R2.NTComponent CorpusTreeRenderProps
subTreeTreeViewCpt = here.ntComponent "subTreeTreeView" cpt where
  cpt (CorpusTreeRenderProps p@{ action
                               , boxes: { handed }
                               , id
                               , render
                               , subTreeParams
                               , tree: NTree (LNode { id: targetId, name, nodeType }) ary }) _ = do
    action' <- T.useLive T.unequal action

    let click e = do
          let action'' = if not validNodeType then Nothing else Just $ SubTreeOut { in: id, out: targetId }
          E.preventDefault  e
          E.stopPropagation e
          T.modify_ (\a -> setTreeOut a action'') action

        children = (map (\ctree -> render (CorpusTreeRenderProps (p { tree = ctree })) []) sortedAry) :: Array R.Element

    pure $

      H.div { className: nodeClass validNodeType }
      [
          H.span
          { className: "subtree__text"
          , on: { click }
          }
          [
            nodeText
            { handed
            , isSelected: isSelected targetId action'
            , name: " " <> name
            } []
          ,
            H.span { className: "subtree__children" }
            children
          ]
      ]
    where
      nodeClass vnt = "subtree__node " <> GT.fldr nodeType true <> " " <> validNodeTypeClass where
        validNodeTypeClass = if vnt then "subtree__node--valid" else ""
      SubTreeParams { valitypes } = subTreeParams
      sortedAry = A.sortWith (\(NTree (LNode {id:id'}) _) -> id')
        $ A.filter (\(NTree (LNode {id:id'}) _) -> id'/= id) ary
      validNodeType = (A.elem nodeType valitypes) && (id /= targetId)
      isSelected n action' = case (subTreeOut action') of
        Nothing                   -> false
        (Just (SubTreeOut {out})) -> n == out
