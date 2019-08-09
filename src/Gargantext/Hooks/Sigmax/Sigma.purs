module Gargantext.Hooks.Sigmax.Sigma where

import Prelude
import Type.Row (class Union)
import Data.Either (Either(..))
import Data.Unit (Unit)
import Effect (Effect)
import FFI.Simple.Objects (named)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4)

foreign import data Sigma :: Type

type NodeRequiredProps = ( id :: String )
type EdgeRequiredProps = ( id :: String, source :: String, target :: String )

class NodeProps (all :: #Type) (extra :: #Type) | all -> extra
class EdgeProps (all :: #Type) (extra :: #Type) | all -> extra

instance nodeProps
  :: Union NodeRequiredProps extra all
  => NodeProps all extra

instance edgeProps
  :: Union EdgeRequiredProps extra all
  => EdgeProps all extra
  
type Graph n e = { nodes :: Array {|n}, edges :: Array {|e} }
type SigmaOpts s = { settings :: s }

sigma :: forall opts err. SigmaOpts opts -> Effect (Either err Sigma)
sigma = runEffectFn3 _sigma Left Right

foreign import _sigma ::
  forall a b opts err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            (SigmaOpts opts)
            (Either err Sigma)

graphRead :: forall node edge err. Sigma -> Graph node edge -> Effect (Either err Unit)
graphRead = runEffectFn4 _graphRead Left Right

foreign import _graphRead ::
  forall a b data_ err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma
            data_
            (Either err Unit)

refresh :: Sigma -> Effect Unit
refresh = runEffectFn1 _refresh

foreign import _refresh :: EffectFn1 Sigma Unit

addRenderer :: forall r err. Sigma -> r -> Effect (Either err Unit)
addRenderer = runEffectFn4 _addRenderer Left Right

foreign import _addRenderer
  :: forall a b r err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma 
            r
            (Either err Unit)

killRenderer :: forall r err. Sigma -> r -> Effect (Either err Unit)
killRenderer = runEffectFn4 _killRenderer Left Right

foreign import _killRenderer
  :: forall a b r err.
  EffectFn4 (a -> Either a b)
            (b -> Either a b)
            Sigma 
            r
            (Either err Unit)

killSigma :: forall err. Sigma -> Effect (Either err Unit)
killSigma = runEffectFn3 _killSigma Left Right

clear :: Sigma -> Effect Unit
clear = runEffectFn1 _clear

foreign import _clear :: EffectFn1 Sigma Unit

foreign import _killSigma
  :: forall a b err.
  EffectFn3 (a -> Either a b)
            (b -> Either a b)
            Sigma 
            (Either err Unit)

bind_ :: forall e. Sigma -> String -> (e -> Effect Unit) -> Effect Unit
bind_ s e h = runEffectFn3 _bind s e (mkEffectFn1 h)

foreign import _bind :: forall e. EffectFn3 Sigma String (EffectFn1 e Unit) Unit

startForceAtlas2 :: forall settings. Sigma -> settings -> Effect Unit
startForceAtlas2 = runEffectFn2 _startForceAtlas2

stopForceAtlas2 :: Sigma -> Effect Unit
stopForceAtlas2 = runEffectFn1 _stopForceAtlas2

killForceAtlas2 :: Sigma -> Effect Unit
killForceAtlas2 = runEffectFn1 _killForceAtlas2

isForceAtlas2Running :: Sigma -> Effect Boolean
isForceAtlas2Running = runEffectFn1 _isForceAtlas2Running

foreign import _startForceAtlas2 :: forall s. EffectFn2 Sigma s Unit
foreign import _stopForceAtlas2 :: EffectFn1 Sigma Unit
foreign import _killForceAtlas2 :: EffectFn1 Sigma Unit
foreign import _isForceAtlas2Running :: EffectFn1 Sigma Boolean

newtype SigmaEasing = SigmaEasing String

sigmaEasing :: { linear :: SigmaEasing
, quadraticIn :: SigmaEasing
, quadraticOut :: SigmaEasing
, quadraticInOut :: SigmaEasing
, cubicIn :: SigmaEasing
, cubicOut :: SigmaEasing
, cubicInOut :: SigmaEasing
}
sigmaEasing =
  { linear : SigmaEasing "linear"
  , quadraticIn : SigmaEasing "quadraticIn"
  , quadraticOut : SigmaEasing "quadraticOut"
  , quadraticInOut : SigmaEasing "quadraticInOut"
  , cubicIn : SigmaEasing "cubicIn"
  , cubicOut : SigmaEasing "cubicOut"
  , cubicInOut : SigmaEasing "cubicInOut"
  }