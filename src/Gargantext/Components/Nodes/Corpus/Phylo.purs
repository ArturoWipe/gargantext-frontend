module Gargantext.Components.Nodes.Corpus.Phylo
  ( phyloLayout
  ) where

import Gargantext.Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import DOM.Simple (document, querySelector)
import DOM.Simple.Console (log2)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FFI.Simple ((..), (.=))
import Gargantext.Components.PhyloExplorer.JSON (PhyloJSONSet)
import Gargantext.Components.PhyloExplorer.Layout (layout)
import Gargantext.Components.PhyloExplorer.Types (PhyloDataSet, parsePhyloJSONSet)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Simple.JSON as JSON
import Toestand as T

type Props =
  ( nodeId :: NodeID
  , session :: Session
  )

phyloLayout :: R2.Component Props
phyloLayout = R.createElement component

componentName :: String
componentName = "Gargantext.Components.Nodes.Corpus.Phylo.Main"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt { nodeId } _ = do

    fetchedData /\ fetchedDataBox <- R2.useBox' (Nothing :: Maybe PhyloDataSet)

    useFirstEffect' do
      -- @XXX: inopinent <div> (see Gargantext.Components.Router) (@TODO?)
      mEl <- querySelector document ".main-page__main-route .container"
      case mEl of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "display") "none"
      -- @XXX: reset "main-page__main-route" wrapper margin
      --       see Gargantext.Components.Router) (@TODO?)
      mEl' <- querySelector document ".main-page__main-route"
      case mEl' of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "padding") "initial"

    useFirstEffect' $ launchAff_ do
      result <- fetchPhyloJSON
      liftEffect $ case result of
        Left err  -> log2 "error" err
        Right res -> T.write_ (Just res) fetchedDataBox

    pure case fetchedData of
      Nothing           -> mempty
      Just phyloDataSet -> layout { phyloDataSet, nodeId } []


fetchPhyloJSON :: Aff (Either String PhyloDataSet)
fetchPhyloJSON =
  let
    -- @WIP remove dumb data
    -- url = "http://localhost:5000/js/knowledge-phylomemy.json"
    -- url = "http://localhost:5000/js/vaccines_countries_06_2021.json"
    url = "js/vaccines_countries_06_2021.json"
    request = AX.defaultRequest
      { url = url
      , method = Left GET
      , responseFormat = ResponseFormat.string
      }
  in do
    result <- request # AX.request
    liftEffect $ case result of
      Left err -> pure $ Left $ AX.printError err
      Right response -> case JSON.readJSON response.body of
        Left err -> pure $ Left $ show err
        Right (res :: PhyloJSONSet) -> pure $ Right $ parsePhyloJSONSet res
