module Gargantext.Utils.Reactix where

import Prelude

import ConvertableOptions as CO
import DOM.Simple as DOM
import DOM.Simple.Console (log2)
import DOM.Simple.Document (document)
import DOM.Simple.Element as Element
import DOM.Simple.Event as DE
import DOM.Simple.Types (class IsNode, class IsElement, DOMRect)
import Data.Array as A
import Data.Either (hush)
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Nullable (Nullable, null, toMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn3)
import FFI.Simple ((..), (...), (.=), defineProperty, delay, args2, args3)
import Partial.Unsafe (unsafePartial)
import React (class ReactPropFields, Children, ReactClass, ReactElement)
import React as React
import Reactix as R
import Reactix.DOM.HTML (ElemFactory, createDOM, text)
import Reactix.DOM.HTML as H
import Reactix.React (react)
import Reactix.SyntheticEvent as RE
import Reactix.Utils (currySecond, hook, tuple)
import Simple.JSON as JSON
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Blob (Blob)
import Web.File.File as WF
import Web.File.FileList (FileList, item)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage, getItem, setItem)

-- | UI Component type with only required props and children
type Component p = Record p -> Array R.Element -> R.Element

-- | UI Component type with only required props and no child
type Leaf p = Record p -> R.Element

-- | UI Component type containing optional props and children
type OptComponent options props provided = CO.Defaults (Record options) (Record provided) (Record props)
  => Record provided -> Array R.Element -> R.Element

-- | UI Component type containing optional props and no child
type OptLeaf options props provided = CO.Defaults (Record options) (Record provided) (Record props)
  => Record provided -> R.Element

component :: forall cpt p. R.IsComponent cpt p (Array R.Element)
  => cpt -> Record p -> Array R.Element -> R.Element
component cpt props children = R.createElement cpt props children

leaf :: forall cpt p. R.IsComponent cpt p (Array R.Element)
  => cpt -> Record p -> R.Element
leaf cpt props = R.createElement cpt props []

optComponent :: forall r r' cpt p.
     CO.Defaults r r' (Record p)
  => R.IsComponent cpt p (Array R.Element)
  => cpt -> r -> r' -> Array R.Element -> R.Element
optComponent cpt options props children = R.createElement cpt props' children where
  props' = CO.defaults options props

optLeaf :: forall r r' cpt p.
     CO.Defaults r r' (Record p)
  => R.IsComponent cpt p (Array R.Element)
  => cpt -> r -> r' -> R.Element
optLeaf cpt options props = R.createElement cpt props' [] where
  props' = CO.defaults options props

-----------------------------------------

type Module = String

type Here =
  { component   :: forall p. String -> R.HooksComponent p -> R.Component p
  , log         :: forall l. l -> Effect Unit
  , log2        :: forall l. String -> l -> Effect Unit
  , ntComponent :: forall p. String -> NTHooksComponent p -> NTComponent p }

here :: Module -> Here
here mod =
  { component:   R.hooksComponentWithModule mod
  , log:         log2 ("[" <> mod <> "]")
  , log2:        \msg -> log2 ("[" <> mod <> "] " <> msg)
  , ntComponent: ntHooksComponentWithModule mod }

-- newtypes
type NTHooksComponent props = props -> Array R.Element -> R.Hooks R.Element
newtype NTComponent p = NTComponent (EffectFn1 p R.Element)

class NTIsComponent component (props :: Type) children
  | component -> props, component -> children where
  ntCreateElement :: component -> props -> children -> R.Element

instance componentIsNTComponent
  :: NTIsComponent (NTComponent props) props (Array R.Element) where
    ntCreateElement = R.rawCreateElement

-- | Turns a `HooksComponent` function into a Component
ntHooksComponent :: forall props. String -> NTHooksComponent props -> NTComponent props
ntHooksComponent name c = NTComponent $ named name $ mkEffectFn1 c'
  where
    c' :: props -> Effect R.Element
    c' props = R.runHooks $ c props (children props)

ntHooksComponentWithModule
 :: forall props. Module -> String -> NTHooksComponent props -> NTComponent props
ntHooksComponentWithModule module' name c =
  ntHooksComponent (module' <> "." <> name) c

---------------------------
-- TODO Copied from reactix, export these:
children :: forall a. a -> Array R.Element
children a = react .. "Children" ... "toArray" $ [ (a .. "children") ]

---------------------------

newtype Point = Point { x :: Number, y :: Number }

-- a reducer function living in effector, for useReductor
type Actor s a = (a -> s -> Effect s)

-- | Turns a ReactElement into aReactix Element
-- | buff (v.) to polish
buff :: ReactElement -> R.Element
buff = unsafeCoerce

-- | Turns a Reactix Element into a ReactElement.
-- | scuff (v.) to spoil the gloss or finish of.
scuff :: R.Element -> ReactElement
scuff = unsafeCoerce

-- class ToElement a where
--   toElement :: a -> R.Element

-- instance ToElement R.Element where
--   toElement = identity

-- instance ToElement ReactElement where
--   toElement = buff

-- instance ToElement a => ToElement (Array a) where
--   toElement = R.fragment <<< map toElement

createElement' :: forall required given
                . ReactPropFields required given
               => ReactClass { children :: Children | required }
               -> Record given -> Array R.Element -> R.Element
createElement' reactClass props children =
  buff $ React.createElement reactClass props $ scuff <$> children

{-
instance isComponentReactClass
      :: R.IsComponent (ReactClass { children :: Children
                                   | props
                                   }) props (Array R.Element) where
  createElement reactClass props children =
    React.createElement reactClass props children
-}

-- | Turns an aff into a useEffect-compatible Effect (Effect Unit)
affEffect :: forall a. String -> Aff a -> Effect (Effect Unit)
affEffect errmsg aff = do
    fiber <- launchAff aff
    pure $ launchAff_ $ killFiber (error errmsg) fiber

mousePosition :: RE.SyntheticEvent DE.MouseEvent -> Point
mousePosition e = Point { x: RE.clientX e, y: RE.clientY e }

mouseClickInElement :: DE.MouseEvent -> DOM.Element -> Boolean
mouseClickInElement e el = x <= cx && cx <= x + width && y <= cy && cy <= y + height
  where
    { x, y, width, height } = Element.boundingRect el
    cx = DE.clientX e
    cy = DE.clientY e

domMousePosition :: DE.MouseEvent -> Point
domMousePosition = mousePosition <<< unsafeCoerce
-- | This is naughty, it quietly mutates the input and returns it
named :: forall o. String -> o -> o
named = flip $ defineProperty "name"

overState :: forall t. (t -> t) -> R.State t -> Effect Unit
overState f (_state /\ setState) = setState f

small :: ElemFactory
small = createDOM "small"

select :: ElemFactory
select = createDOM "select"

menu :: ElemFactory
menu = createDOM "menu"

frame :: ElemFactory
frame = createDOM "frame"

frameset :: ElemFactory
frameset = createDOM "frameset"

keyCode :: forall event. event -> Effect Int
keyCode = runEffectFn1 _keyCode

foreign import _keyCode
  :: forall e. EffectFn1 e Int

nullRef :: forall t. R.Hooks (R.Ref (Nullable t))
nullRef = R.useRef null

nothingRef :: forall t. R.Hooks (R.Ref (Maybe t))
nothingRef = R.useRef Nothing

useLayoutEffect1' :: forall a. a -> (Unit -> Effect Unit) -> R.Hooks Unit
useLayoutEffect1' a f = R.useLayoutEffect1 a $ do
  liftEffect $ f unit
  pure $ pure unit

useLayoutRef :: forall a b. (a -> b) -> b -> R.Ref a -> R.Hooks (R.Ref b)
useLayoutRef fn init ref = do
  new <- R.useRef init
  let old = R.readRef ref
  useLayoutEffect1' old $ \_ -> R.setRef new (fn old)
  pure new

usePositionRef :: R.Ref (Nullable DOM.Element) -> R.Hooks (R.Ref (Maybe DOM.DOMRect))
usePositionRef = useLayoutRef (map Element.boundingRect <<< toMaybe) Nothing

readPositionRef :: R.Ref (Nullable DOM.Element) -> Maybe DOM.DOMRect
readPositionRef el = do
  let posRef = R.readRef el
  Element.boundingRect <$> toMaybe posRef

getElementById :: String -> Effect (Maybe DOM.Element)
getElementById = (flip delay) h
  where
    h id = pure $ toMaybe $ document ... "getElementById" $ [id]

-- We just assume it works, so make sure it's in the html
getPortalHost :: R.Hooks DOM.Element
getPortalHost = R.unsafeHooksEffect $ delay unit $ \_ -> pure $ document ... "getElementById" $ ["portal"]

useLayoutEffectOnce :: Effect (Effect Unit) -> R.Hooks Unit
useLayoutEffectOnce e = R.unsafeUseLayoutEffect e []

singleParent :: forall props. R.Component props -> Record props -> R.Element -> R.Element
singleParent cpt props child = R.createElement cpt props [ child ]

childless :: forall props. R.Component props -> Record props -> R.Element
childless cpt props = R.createElement cpt props []

showText :: forall s. Show s => s -> R.Element
showText = text <<< show

----- Reactix's new effectful reducer: sneak-peek because anoe wants to demo on tuesday

-- | Like a reducer, but lives in Effect
type Reductor state action = Tuple state (action -> Effect Unit)

-- | Like useReductor, but lives in Effect
useReductor :: forall s a i. Actor s a -> (i -> Effect s) -> i -> R.Hooks (Reductor s a)
useReductor f i j =
  hook $ \_ ->
    pure $ currySecond $ tuple $ react ... "useReducer" $ args3 (mkEffectFn2 (flip f)) j (mkEffectFn1 i)

-- | Like `useReductor`, but takes an initial state instead of an
-- | initialiser function and argument
useReductor' :: forall s a. Actor s a -> s -> R.Hooks (Reductor s a)
useReductor' r = useReductor r pure

render :: R.Element -> DOM.Element -> Effect Unit
render e d = delay unit $ \_ -> pure $ R.reactDOM ... "render" $ args2 e d

addRootElement :: DOM.Element -> Effect Unit
addRootElement = runEffectFn1 _addRootElement

foreign import _addRootElement
  :: EffectFn1 DOM.Element Unit

appendChild :: forall n m. IsNode n => IsNode m => n -> m -> Effect Unit
appendChild n c = delay unit $ \_ -> pure $ n ... "appendChild" $ [c]

appendChildToParentId :: forall c. IsNode c => String -> c -> Effect Unit
appendChildToParentId ps c = delay unit $ \_ -> do
  parentEl <- getElementById ps
  case parentEl of
    Nothing -> pure unit
    Just el -> appendChild el c

effectLink :: Effect Unit -> String -> R.Element
effectLink eff msg = H.a { on: {click: const eff} } [H.text msg]

useCache :: forall i o. Eq i => i -> (i -> R.Hooks o) -> R.Hooks o
useCache i f = do
  iRef <- R.useRef Nothing
  oRef <- R.useRef Nothing
  let currI = R.readRef iRef
  let currO = R.readRef oRef
  if currI == Just i then
    case currO of
      Nothing -> f i -- this one shouldn't happen, but purescript
      Just v -> pure v
  else do
    new <- f i
    R.unsafeHooksEffect (R.setRef iRef $ Just i)
    R.unsafeHooksEffect (R.setRef oRef $ Just new)
    pure new

inputFile :: forall e. Int -> e -> Maybe WF.File
inputFile n e = item n $ ((el .. "files") :: FileList)
  where
    el = e .. "target"

-- | Get blob from an 'onchange' e.target event
inputFileBlob n e = unsafePartial $ do
  let ff = fromJust $ inputFile n e
  pure $ WF.toBlob ff

inputFileNameWithBlob :: forall e. Int -> e -> Maybe {blob :: Blob, name :: String}
inputFileNameWithBlob n e = case ff of
    Nothing -> Nothing
    Just f  -> Just {blob: WF.toBlob f, name: WF.name f}
  where
    ff = inputFile n e

-- | Get blob from a drop event
--dataTransferFileBlob :: forall e. DE.IsEvent e => RE.SyntheticEvent e -> Effect Blob
dataTransferFileBlob e = unsafePartial $ do
    let ff = fromJust $ item 0 $ ((e .. "dataTransfer" .. "files") :: FileList)
    pure $ WF.toBlob ff

blur :: DOM.Element -> Effect Unit
blur el = el ... "blur" $ []

row :: Array R.Element -> R.Element
row children = H.div { className: "row" } children

col :: Int -> Array R.Element -> R.Element
col n children = H.div { className : "col-md-" <> show n } children

innerText :: DOM.Element -> String
innerText e = e .. "innerText"

foreign import data Selection :: Type

getSelection :: Unit -> Effect Selection
getSelection = runEffectFn1 _getSelection

foreign import _getSelection :: EffectFn1 Unit Selection

stringify :: forall a. a -> Int -> String
stringify j indent = runFn2 _stringify j indent

foreign import _stringify :: forall a. Fn2 a Int String

getls :: Effect Storage
getls = window >>= localStorage

openNodesKey :: LocalStorageKey
openNodesKey = "garg-open-nodes"

type LocalStorageKey = String

loadLocalStorageState :: forall s. JSON.ReadForeign s => LocalStorageKey -> T.Box s -> Effect Unit
loadLocalStorageState key cell = do
  storage <- getls
  item :: Maybe String <- getItem key storage
  -- let json = hush <<< Argonaut.jsonParser =<< item
  -- let parsed = hush <<< Argonaut.decodeJson =<< json
  let parsed = hush <<< JSON.readJSON $ fromMaybe "" item
  case parsed of
    Nothing -> pure unit
    Just p  -> void $ T.write p cell

listenLocalStorageState :: forall s. JSON.WriteForeign s => LocalStorageKey -> T.Change s -> Effect Unit
listenLocalStorageState key { old, new } = do
  --let json = Json.stringify $ Argonaut.encodeJson new
  let json = JSON.writeJSON new
  storage <- getls
  setItem key json storage

getMessageDataStr :: DE.MessageEvent -> String
getMessageDataStr = getMessageData

getMessageOrigin :: DE.MessageEvent -> String
getMessageOrigin me = me .. "origin"

getMessageData :: forall o. DE.MessageEvent -> o
getMessageData me = me .. "data"

foreign import _postMessage
  :: forall r. EffectFn3 r String String Unit

postMessage :: forall r. R.Ref (Nullable r) -> String -> Effect Unit
postMessage ref msg = do
  case (R.readNullableRef ref) of
    (Just ifr) -> do
      runEffectFn3 _postMessage ifr msg (ifr .. "src")
    (Nothing) -> pure unit

foreign import _setCookie :: EffectFn1 String Unit

setCookie :: String -> Effect Unit
setCookie = runEffectFn1 _setCookie

focus :: Nullable R.Element -> Effect Unit
focus nEl = case toMaybe nEl of
  Nothing -> pure unit
  Just el -> el ... "focus" $ []

setIndeterminateCheckbox :: R.Element -> Boolean -> Effect R.Element
setIndeterminateCheckbox el val = pure $ (el .= "indeterminate") val


-- A "trigger" is a ref to a function which is used to make changes without
-- modifying too much DOM.
-- This is to escape passing explicit state to nested child components.
type Trigger a = R.Ref (Maybe (a -> Effect Unit))

callTrigger :: forall a. Trigger a -> a -> Effect Unit
callTrigger tRef arg = case R.readRef tRef of
  Nothing -> do
    log2 "[callTrigger] trigger is empty" tRef
    pure unit
  Just t  -> t arg

setTrigger :: forall a. Trigger a -> (a -> Effect Unit) -> Effect Unit
setTrigger tRef fun = R.setRef tRef $ Just fun

clearTrigger :: forall a. Trigger a -> Effect Unit
clearTrigger tRef = R.setRef tRef Nothing

type Rect =
  ( x :: Number
  , y :: Number
  , width :: Number
  , height :: Number )

foreign import _domRectFromRect :: Fn1 (Record Rect) DOMRect

domRectFromRect :: Record Rect -> DOMRect
domRectFromRect = runFn1 _domRectFromRect

boundingRect :: forall e. IsElement e => Array e -> DOMRect
boundingRect els =
  case A.uncons els of
    Nothing -> domRectFromRect { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }
    Just { head, tail } ->
      let br = Element.boundingRect head
      in
      case tail of
        [] -> br
        _  ->
          let brs = boundingRect tail
              minx = min br.left brs.left
              maxx = max br.right brs.right
              miny = min br.top brs.top
              maxy = max br.bottom brs.bottom
          in
           domRectFromRect { x: minx
                           , y: miny
                           , width: maxx - minx
                           , height: maxy - miny }

--------------------------------------


-- | One-liner `if` simplifying render writing
-- | (best for one child)
if' :: Boolean -> R.Element -> R.Element
if' = if _ then _ else mempty

-- | One-liner `if` simplifying render writing
-- | (best for multiple children)
if_ :: Boolean -> Array (R.Element) -> R.Element
if_ pred arr = if pred then (R.fragment arr) else mempty

-- | Toestand `useLive` automatically sets to "unchanged" behavior
useLive' :: forall box b. T.Read box b => Eq b => box -> R.Hooks b
useLive' = T.useLive T.unequal

-- | Toestand `useBox` + `useLive'` shorthand following same patterns as
-- | React StateHooks API
useBox' :: forall b. Eq b => b -> R.Hooks (Tuple b (T.Box b))
useBox' default = do
  box <- T.useBox default
  b <- useLive' box
  pure $ b /\ box
