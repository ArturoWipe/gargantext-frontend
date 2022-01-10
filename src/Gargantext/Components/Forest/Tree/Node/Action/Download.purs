module Gargantext.Components.Forest.Tree.Node.Action.Download where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Common (toLower)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(DownloadNode))
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, panel, submitButtonHref)
import Gargantext.Ends (url)
import Gargantext.Prelude
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Documentation"

-- | Action : Download
type ActionDownload =
  ( id       :: ID
  , nodeType :: GT.NodeType
  , session  :: Session )

actionDownload :: R2.Component ActionDownload
actionDownload = R.createElement actionDownloadCpt
actionDownloadCpt :: R.Component ActionDownload
actionDownloadCpt = here.component "actionDownload" cpt where
  cpt props@{ nodeType: GT.Corpus } _    = pure $ actionDownloadCorpus props []
  cpt props@{ nodeType: GT.Graph } _     = pure $ actionDownloadGraph props []
  cpt props@{ nodeType: GT.NodeList } _  = pure $ actionDownloadNodeList props []
  cpt props@{ nodeType: GT.NodeTexts } _ = pure $ actionDownloadNodeTexts props []
  cpt props@{ nodeType: _ } _            = pure $ actionDownloadOther props []

actionDownloadCorpus :: R2.Component ActionDownload
actionDownloadCorpus = R.createElement actionDownloadCorpusCpt
actionDownloadCorpusCpt :: R.Component ActionDownload
actionDownloadCorpusCpt = here.component "actionDownloadCorpus" cpt where
  cpt { id, session } _ = do
    pure $ panel [H.div {} [H.text info]]
      (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Corpus (Just id) "export"
      info  = "Download as JSON"

actionDownloadGraph :: R2.Component ActionDownload
actionDownloadGraph = R.createElement actionDownloadGraphCpt
actionDownloadGraphCpt :: R.Component ActionDownload
actionDownloadGraphCpt = here.component "actionDownloadGraph" cpt where
  cpt { id, session } _ = do
    pure $ panel [H.div {} [H.text info]]
      (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Graph (Just id) "gexf"
      info  = "Info about the Graph as GEXF format"

actionDownloadNodeList :: R2.Component ActionDownload
actionDownloadNodeList = R.createElement actionDownloadNodeListCpt
actionDownloadNodeListCpt :: R.Component ActionDownload
actionDownloadNodeListCpt = here.component "actionDownloadNodeList" cpt where
  cpt { id, session } _ = do
    pure $ panel [ H.div {} [H.text info] ]
      (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.NodeList (Just id) ""
      info  = "Info about the List as JSON format"

data NodeTextsDownloadFormat = CSV | JSON
derive instance Eq NodeTextsDownloadFormat
derive instance Generic NodeTextsDownloadFormat _
instance Show NodeTextsDownloadFormat where show = genericShow

actionDownloadNodeTexts :: R2.Component ActionDownload
actionDownloadNodeTexts = R.createElement actionDownloadNodeTextsCpt
actionDownloadNodeTextsCpt :: R.Component ActionDownload
actionDownloadNodeTextsCpt = here.component "actionDownloadNodeTexts" cpt where
  cpt { id, session } _ = do
    downloadFormat <- T.useBox JSON
    downloadFormat' <- T.useLive T.unequal downloadFormat
    
    pure $ panel
      [ R2.select { className: "form-control" }
        [ opt CSV downloadFormat downloadFormat'
        , opt JSON downloadFormat downloadFormat' ]
      , H.div {} [ H.text $ info downloadFormat' ]
      ]
      (submitButtonHref DownloadNode $ href downloadFormat')
    where
      opt t downloadFormat df = H.option { on: { click: onClick }
                                         , selected: df == t } [ H.text $ show t ]
        where
          onClick _ = T.write_ t downloadFormat
      href t  = url session $ Routes.NodeAPI GT.NodeTexts (Just id) ("export/" <> (toLower $ show t))
      info t  = "Info about the Documents as " <> show t <> " format"

{-
-- TODO fix the route
actionDownload GT.Texts id session = pure $ panel [H.div {} [H.text info]]
                                           (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Texts (Just id) ""
      info  = "TODO: fix the backend route. What is the expected result ?"
      -}
actionDownloadOther :: R2.Component ActionDownload
actionDownloadOther = R.createElement actionDownloadOtherCpt
actionDownloadOtherCpt :: R.Component ActionDownload
actionDownloadOtherCpt = here.component "actionDownloadOther" cpt where
  cpt _ _ = do
    pure $ fragmentPT $ "Soon, you will be able to download your file here "
