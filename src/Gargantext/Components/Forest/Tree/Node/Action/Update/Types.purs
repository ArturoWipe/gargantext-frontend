module Gargantext.Components.Forest.Tree.Node.Action.Update.Types where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Data.Maybe (Maybe(..))
import Gargantext.Prelude (class Eq, class Read, class Show)


data UpdateNodeParams = UpdateNodeParamsList  { method :: Method }
                      | UpdateNodeParamsGraph { method :: Metric }
                      | UpdateNodeParamsTexts { method :: Granularity }
                      | UpdateNodeParamsBoard { method :: Charts }

derive instance eqUpdateNodeParams :: Eq UpdateNodeParams

derive instance genericUpdateNodeParams :: Generic UpdateNodeParams _

instance showUpdateNodeParams :: Show UpdateNodeParams where
  show = genericShow

instance decodeJsonUpdateNodeParams :: Argonaut.DecodeJson UpdateNodeParams where
  decodeJson = genericSumDecodeJson

instance encodeJsonUpdateNodeParams :: Argonaut.EncodeJson UpdateNodeParams where
  encodeJson = genericSumEncodeJson

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel

derive instance genericMethod :: Generic Method _

derive instance eqMethod :: Eq Method

instance showMethod :: Show Method where
  show = genericShow

instance readMethod :: Read Method where
  read "Basic"    = Just Basic
  read "Advanced" = Just Advanced
  read "WithModel" = Just WithModel
  read _           = Nothing

instance decodeJsonMethod :: Argonaut.DecodeJson Method where
  decodeJson = genericSumDecodeJson

instance encodeJsonMethod :: Argonaut.EncodeJson Method where
  encodeJson = genericSumEncodeJson

----------------------------------------------------------------------
data Metric = Order1 | Order2

derive instance genericMetric :: Generic Metric _

derive instance eqMetric :: Eq Metric

instance showMetric :: Show Metric where
  show = genericShow

instance readMetric :: Read Metric where
  read "Order1"    = Just Order1
  read "Order2"    = Just Order2
  read _           = Nothing

instance decodeJsonMetric :: Argonaut.DecodeJson Metric where
  decodeJson = genericSumDecodeJson

instance encodeJsonMetric :: Argonaut.EncodeJson Metric where
  encodeJson = genericSumEncodeJson

----------------------------------------------------------------------

data Granularity = NewNgrams | NewTexts | Both

derive instance genericGranularity :: Generic Granularity _

derive instance eqGranularity :: Eq Granularity

instance showGranularity :: Show Granularity where
  show = genericShow

instance readGranularity :: Read Granularity where
  read "NewNgrams" = Just NewNgrams
  read "NewTexts"  = Just NewTexts
  read "Both"      = Just Both
  read _           = Nothing

instance decodeJsonGranularity :: Argonaut.DecodeJson Granularity where
  decodeJson = genericSumDecodeJson

instance encodeJsonGranularity :: Argonaut.EncodeJson Granularity where
  encodeJson = genericSumEncodeJson


----------------------------------------------------------------------
data Charts = AllCharts

derive instance genericChart :: Generic Charts _

derive instance eqChart :: Eq Charts

instance showChart :: Show Charts where
  show = genericShow

instance readChart :: Read Charts where
  read "AllCharts"  = Just AllCharts
  read _           = Nothing

instance decodeJsonChart :: Argonaut.DecodeJson Charts where
  decodeJson = genericSumDecodeJson

instance encodeJsonChart :: Argonaut.EncodeJson Charts where
  encodeJson = genericSumEncodeJson

