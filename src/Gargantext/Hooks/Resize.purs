module Gargantext.Hooks.Resize
  ( useResizeHandler
  , ResizeType(..)
  ) where

import Gargantext.Prelude

import DOM.Simple (Document, Window, document, window)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Extra (kebabCase)
import Effect (Effect)
import Effect.Uncurried (EffectFn5, runEffectFn5)
import Prelude (class Show)
import Reactix as R

foreign import _resize :: EffectFn5
  Window
  Document
  String
  String
  String
  Unit

resize ::
     Window
  -> Document
  -> String
  -> String
  -> String
  -> Effect Unit
resize = runEffectFn5 _resize

-- @TODO: use Ref instead of Selectors
type Input =
  ( source :: String -- selector
  , target :: String -- selector
  , type   :: ResizeType
  )

type Output = Record Input -> Effect Unit


data ResizeType
  = Vertical
  | Horizontal
  | Both

derive instance Generic ResizeType _
derive instance Eq ResizeType
instance Show ResizeType where
  show = kebabCase <<< genericShow


useResizeHandler :: R.Hooks Output
useResizeHandler = do
  let
    output { source, target, type: t }
      = resize
        window
        document
        source
        target
        (show t)

  pure output
