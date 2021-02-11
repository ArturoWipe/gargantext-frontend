module Gargantext.Components.GraphExplorer.Button
  ( centerButton
  , Props
  , simpleButton
  , cameraButton
  ) where

import Prelude

import Data.DateTime as DDT
import Data.DateTime.Instant as DDI
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple (snd)
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadArbitraryDataURL)
import Gargantext.Components.GraphExplorer.API (cloneGraph)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.GraphExplorer.Utils as GEU
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.GraphExplorer.Button"

type Props = (
    onClick :: forall e. e -> Effect Unit
  , text :: String
  )

simpleButton :: Record Props -> R.Element
simpleButton props = R.createElement simpleButtonCpt props []

simpleButtonCpt :: R.Component Props
simpleButtonCpt = R.hooksComponentWithModule thisModule "simpleButton" cpt
  where
    cpt {onClick, text} _ = do
      pure $ H.button { className: "btn btn-outline-primary"
                      , on: {click: onClick}
                      } [ R2.small {} [ H.text text ] ]

centerButton :: R.Ref Sigmax.Sigma -> R.Element
centerButton sigmaRef = simpleButton {
    onClick: \_ -> do
      let sigma = R.readRef sigmaRef
      Sigmax.dependOnSigma sigma "[centerButton] sigma: Nothing" $ \s ->
        Sigma.goToAllCameras s {x: 0.0, y: 0.0, ratio: 1.0, angle: 0.0}
  , text: "Center"
  }


type CameraButtonProps = (
    asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , id :: Int
  , hyperdataGraph :: GET.HyperdataGraph
  , session :: Session
  , sigmaRef :: R.Ref Sigmax.Sigma
  , treeReload :: Unit -> Effect Unit
  )


cameraButton :: Record CameraButtonProps -> R.Element
cameraButton { asyncTasksRef
             , id
             , hyperdataGraph: GET.HyperdataGraph { graph: GET.GraphData hyperdataGraph }
             , session
             , sigmaRef
             , treeReload } = simpleButton {
    onClick: \_ -> do
      let sigma = R.readRef sigmaRef
      Sigmax.dependOnSigma sigma "[cameraButton] sigma: Nothing" $ \s -> do
        screen <- Sigma.takeScreenshot s
        now <- EN.now
        let nowdt = DDI.toDateTime now
            nowd = DDT.date nowdt
            nowt = DDT.time nowdt
            nowStr = DS.joinWith "-" [ show $ fromEnum $ DDT.year nowd
                                     , show $ fromEnum $ DDT.month nowd
                                     , show $ fromEnum $ DDT.day nowd
                                     , show $ fromEnum $ DDT.hour nowt
                                     , show $ fromEnum $ DDT.minute nowt
                                     , show $ fromEnum $ DDT.second nowt ]
        edges <- Sigmax.getEdges s
        nodes <- Sigmax.getNodes s
        let graphData = GET.GraphData $ hyperdataGraph { edges = map GEU.stEdgeToGET edges
                                                       , nodes = GEU.normalizeNodes $ map GEU.stNodeToGET nodes }
        let cameras = map Sigma.toCamera $ Sigma.cameras s
        let camera = case cameras of
              [c] -> GET.Camera { ratio: c.ratio
                                , x: c.x
                                , y: c.y }
              _   -> GET.Camera { ratio: 1.0
                               , x: 0.0
                               , y: 0.0 }
        let hyperdataGraph = GET.HyperdataGraph { graph: graphData
                                                , mCamera: Just camera }
        launchAff_ $ do
          clonedGraphId <- cloneGraph { id, hyperdataGraph, session }
          task <- uploadArbitraryDataURL session clonedGraphId (Just $ nowStr <> "-" <> "screenshot.png") screen
          case R.readRef asyncTasksRef of
            Nothing -> pure unit
            Just asyncTasks -> liftEffect $ do
              snd asyncTasks $ GAT.Insert id task
              treeReload unit
          pure task
  , text: "Screenshot"
  }
