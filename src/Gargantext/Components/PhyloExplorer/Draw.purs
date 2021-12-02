module Gargantext.Components.PhyloExplorer.Draw
  ( drawPhylo
  , highlightSource
  , unhide
  , setGlobalDependencies, setGlobalD3Reference
  ) where

import Gargantext.Prelude

import DOM.Simple (Document, Window, querySelectorAll)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn7, runEffectFn1, runEffectFn7)
import FFI.Simple (applyTo, getProperty, (..), (.=), (.?))
import Gargantext.Components.PhyloExplorer.Types (AncestorLink, Branch, BranchLink, GlobalTerm(..), Group(..), Link, Period, PhyloDataSet(..))
import Graphics.D3.Base (D3, D3Eff)
import Graphics.D3.Selection as D3S
import Graphics.D3.Util (ffi)

foreign import _drawPhylo :: EffectFn7
  (Array Branch)
  (Array Period)
  (Array Group)
  (Array Link)
  (Array AncestorLink)
  (Array BranchLink)
  (Array Number)
  (Unit)

drawPhylo ::
     Array Branch
  -> Array Period
  -> Array Group
  -> Array Link
  -> Array AncestorLink
  -> Array BranchLink
  -> Array Number
  -> Effect Unit
drawPhylo = runEffectFn7 _drawPhylo

foreign import _drawWordCloud :: forall a. EffectFn1 (Array a) Unit

drawWordCloud :: forall a. Array a -> Effect Unit
drawWordCloud = runEffectFn1 _drawWordCloud

-----------------------------------------------------------

orDie :: forall err a. Maybe a -> err -> Either err a
orDie (Just a) _   = Right a
orDie Nothing  err = Left err

-- @XXX: FFI.Simple `(...)` throws error (JavaScript issue)
--       need to decompose computation
--
--       (?) chained prototype property issue?
applyTo_ :: forall src arg res. src -> String -> Array arg -> res
applyTo_ src name args =
  let fn = getProperty name src
  in applyTo fn src args

infixl 4 applyTo_ as ~~

-- @WIP: DOM.Simple lack of "ClassList" module
addClass :: forall el. el -> Array String -> Effect Unit
addClass el args = pure $ (el .. "classList") ~~ "add" $ args

removeClass :: forall el. el -> Array String -> Effect Unit
removeClass el args = pure $ (el .. "classList") ~~ "remove" $ args

-- @WIP: "Graphics.D3.Selection" lack of "filter" function
-- @WIP: "Graphics.D3.Selection" lack of "nodes" function
selectionFilter :: forall d. String -> D3S.Selection d -> D3Eff (D3S.Selection D3S.Void)
selectionFilter = ffi ["query", "selection", ""] "selection.filter(query)"

selectionNodes :: forall d el. D3S.Selection d -> D3Eff (Array el)
selectionNodes = ffi ["selection", ""] "selection.nodes()"

-----------------------------------------------------------

setGlobalDependencies :: Window -> PhyloDataSet -> Effect Unit
setGlobalDependencies w (PhyloDataSet o)
  = do
    _ <- pure $ (w .= "freq") {}
    _ <- pure $ (w .= "nbBranches") o.nbBranches
    _ <- pure $ (w .= "nbDocs") o.nbDocs
    _ <- pure $ (w .= "nbFoundations") o.nbFoundations
    _ <- pure $ (w .= "nbGroups") o.nbGroups
    _ <- pure $ (w .= "nbPeriods") o.nbPeriods
    _ <- pure $ (w .= "nbTerms") o.nbTerms
    _ <- pure $ (w .= "sources") o.sources
    _ <- pure $ (w .= "terms") []
    _ <- pure $ (w .= "timeScale") o.timeScale
    _ <- pure $ (w .= "weighted") o.weighted

    (freq :: Array Int)         <- pure $ w .. "freq"
    (terms :: Array GlobalTerm) <- pure $ w .. "terms"

    for_ o.groups \(Group g) -> do

      let
        f = g.foundation
        l = g.label

      forWithIndex_ f \idx val ->
        let
          idx' = show idx
          val' = show val
        in
          -- For each entries in group.foundation array,
          -- increment consequently the global window.keys array
          case (freq .? val') of
            Nothing -> pure $ (freq .= val') 0
            Just v  -> pure $ (freq .= val') (v +1)
        *>
          -- For each entries in group.foundation array,
          -- if the global window.terms does not have it in property,
          -- append an item to the global window.terms
          case (terms .? val') of
            Just _  -> pure unit
            Nothing -> void <<< pure $ (terms .= val') $ GlobalTerm
              { label: l .. idx'
              , fdt  : val'
              }

    -- Use FFI native `Array.flat` method (not mutating its caller in this
    -- context)
    void do
      new <- pure $ (terms ~~ "flat") []
      pure $ (w .= "terms") new

-- @XXX: prevent PureScript from not injecting D3
setGlobalD3Reference :: Window -> D3 -> Effect Unit
setGlobalD3Reference window d3 = void $ pure $ (window .= "d3") d3

-----------------------------------------------------------

unhide :: Document -> String -> Effect Unit
unhide d s = do
  setText s   `toElements` "#phyloName"
  turnVisible `toElements` "#phyloName"
  turnVisible `toElements` "#phyloTopBar"
  turnVisible `toElements` ".reset"
  turnVisible `toElements` ".label"
  turnVisible `toElements` ".heading"
  turnVisible `toElements` ".export"

  where
    toElements fn query = querySelectorAll d query >>= flip for_ fn

    turnVisible el = do
      style <- pure $ (el .. "style")
      pure $ (style .= "visibility") "visible"

    setText name el = pure $ (el .= "innerHTML") name

-----------------------------------------------------------

highlightSource :: Window -> String -> Effect Unit
highlightSource window value =
  let
    hasHighlight = maybe false identity (window .? "highlighted")
    hasLdView    = maybe false identity (window .? "ldView")

  in do
    groups <- D3S.rootSelectAll ".group-inner"

    if hasHighlight
    then
          selectionFilter ".source-focus" groups
      >>= selectionNodes
      >>= flip for_ (flip addClass [ "group-unfocus" ])
    else
      pure unit


    -- unselected all the groups
    _ <-  selectionNodes groups
      >>= flip for_ (flip removeClass [ "source-focus" ])

    if hasLdView
    then
          selectionNodes groups
      >>= flip for_ (fill "#f5eee6")
    else
          selectionNodes groups
      >>= flip for_ (fill "#fff")

    _ <-  D3S.rootSelectAll ".peak"
      >>= D3S.classed "peak-focus-source" false


    -- select the relevant ones
    if (value == "unselect")
    then
      pure unit
    else do
      arr <- selectionFilter (".source-" <> value) groups
        >>= selectionNodes

      drawWordCloud arr
      for_ arr selectNodeGroup

  where

    fill :: forall el. String -> el -> Effect Unit
    fill hex el = do
      style <- pure $ (el .. "style")
      pure $ (style .= "fill") hex


    selectNodeGroup :: forall el. el -> Effect Unit
    selectNodeGroup el = do
      removeClass el [ "group-unfocus" ]
      addClass el [ "source-focus" ]
      fill "#a6bddb" el

      bid <- pure $ (el ~~ "getAttribute") [ "bId" ]

      void $
            D3S.rootSelect ("#peak-" <> bid)
        >>= D3S.classed "peak-focus-source" true
