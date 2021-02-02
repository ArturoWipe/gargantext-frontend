module Gargantext.Components.Nodes.Annuaire.User.Contact
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , contactLayout
  )
  where

import DOM.Simple.Console (log2)
import Data.Lens as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs as Tabs
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (Contact'(..), ContactData', ContactTouch(..), ContactWhere(..), ContactWho(..), HyperdataContact(..), HyperdataUser(..), _city, _country, _firstName, _labTeamDeptsJoinComma, _lastName, _mail, _office, _organizationJoinComma, _ouFirst, _phone, _role, _shared, _touch, _who, defaultContactTouch, defaultContactWhere, defaultContactWho, defaultHyperdataContact, defaultHyperdataUser)
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, const, discard, pure, show, unit, ($), (+), (<$>), (<<<), (<>), (==))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Annuaire.User.Contacts"

type DisplayProps = (
  title :: String
  )

display :: R2.Component DisplayProps
display = R.createElement displayCpt

displayCpt :: R.Component DisplayProps
displayCpt = R.hooksComponentWithModule thisModule "display" cpt
  where
    cpt { title } children = do
      pure $ H.div { className: "container-fluid" }
        [ H.div { className: "row", id: "contact-page-header" }
          [ H.div { className: "col-md-6"} [ H.h3 {} [ H.text title ] ]
          , H.div { className: "col-md-8"} []
          , H.div { className: "col-md-2"} [ H.span {} [ H.text "" ] ]
          ]
        , H.div { className: "row", id: "contact-page-info" }
          [ H.div { className: "col-md-12" }
            [ H.div { className: "row" }
              [ H.div { className: "col-md-2" } [ H.img { src: "/images/Gargantextuel-212x300.jpg"} ]
              , H.div { className: "col-md-1"} []
              , H.div { className: "col-md-8"} children
              ]]]]

-- | TODO format data in better design (UI) shape
contactInfos :: HyperdataContact -> (HyperdataContact -> Effect Unit) -> Array R.Element
contactInfos h onUpdateHyperdata = item <$> contactInfoItems
  where
    item {label, defaultVal, lens} =
      contactInfoItem { hyperdata: h
                      , label
                      , lens
                      , onUpdateHyperdata
                      , placeholder: defaultVal }

contactInfoItems :: Array {label:: String, defaultVal:: String, lens:: HyperdataContactLens}
contactInfoItems =
  [ {label: "Last Name"    , defaultVal: "Empty Last Name"    , lens: _who     <<< _lastName             }
  , {label: "First Name"   , defaultVal: "Empty First Name"   , lens: _who     <<< _firstName            }
  , {label: "Organisation" , defaultVal: "Empty Organisation" , lens: _ouFirst <<< _organizationJoinComma}
  , {label: "Lab/Team/Dept", defaultVal: "Empty Lab/Team/Dept", lens: _ouFirst <<< _labTeamDeptsJoinComma}
  , {label: "Office"       , defaultVal: "Empty Office"       , lens: _ouFirst <<< _office               }
  , {label: "City"         , defaultVal: "Empty City"         , lens: _ouFirst <<< _city                 }
  , {label: "Country"      , defaultVal: "Empty Country"      , lens: _ouFirst <<< _country              }
  , {label: "Role"         , defaultVal: "Empty Role"         , lens: _ouFirst <<< _role                 }
  , {label: "Phone"        , defaultVal: "Empty Phone"        , lens: _ouFirst <<< _touch <<< _phone     }
  , {label: "Mail"         , defaultVal: "Empty Mail"         , lens: _ouFirst <<< _touch <<< _mail      }
  ]

type HyperdataContactLens = L.ALens' HyperdataContact String

type ContactInfoItemProps =
  ( hyperdata         :: HyperdataContact
  , label             :: String
  , lens              :: HyperdataContactLens
  , onUpdateHyperdata :: HyperdataContact -> Effect Unit
  , placeholder       :: String
  )

contactInfoItem :: Record ContactInfoItemProps -> R.Element
contactInfoItem props = R.createElement contactInfoItemCpt props []

contactInfoItemCpt :: R.Component ContactInfoItemProps
contactInfoItemCpt = R.hooksComponentWithModule thisModule "contactInfoItem" cpt
  where
    cpt {hyperdata, label, lens, onUpdateHyperdata, placeholder} _ = do
      isEditing <- R.useState' false
      let value = (L.view cLens hyperdata) :: String
      valueRef <- R.useRef value

      pure $ H.div { className: "form-group row" } [
        H.span { className: "col-sm-2 col-form-label" } [ H.text label ]
      , item isEditing valueRef
      ]

      where
        cLens = L.cloneLens lens
        item (false /\ setIsEditing) valueRef =
          H.div { className: "input-group col-sm-6" } [
            H.input { className: "form-control"
                    , defaultValue: placeholder
                    , disabled: 1
                    , type: "text" }
          , H.div { className: "btn input-group-append"
                  , on: { click: onClick } } [
              H.div { className: "input-group-text fa fa-pencil" } []
            ]
          ]
          where
            placeholder = R.readRef valueRef
            onClick _ = setIsEditing $ const true
        item (true /\ setIsEditing) valueRef =
          H.div { className: "input-group col-sm-6" } [
            inputWithEnter {
                autoFocus: true
              , className: "form-control"
              , defaultValue: R.readRef valueRef
              , onEnter: onClick
              , onValueChanged: R.setRef valueRef
              , placeholder
              , type: "text"
              }
          , H.div { className: "btn input-group-append"
                  , on: { click: onClick } } [
              H.div { className: "input-group-text fa fa-floppy-o" } []
            ]
          ]
          where
            onClick _ = do
              setIsEditing $ const false
              let newHyperdata = (L.over cLens (\_ -> R.readRef valueRef) hyperdata) :: HyperdataContact
              onUpdateHyperdata newHyperdata

listElement :: Array R.Element -> R.Element
listElement = H.li { className: "list-group-item justify-content-between" }

type LayoutProps =
  ( appReload     :: GUR.ReloadS
  , asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , frontends     :: Frontends
  , nodeId        :: Int
  , session       :: Session
  , treeReloadRef :: GUR.ReloadWithInitializeRef
  )

type KeyLayoutProps = (
    key :: String
  | LayoutProps
  )


saveContactHyperdata :: Session -> Int -> HyperdataContact -> Aff Int
saveContactHyperdata session id h = do
  put session (Routes.NodeAPI Node (Just id) "") h


type AnnuaireLayoutProps = ( annuaireId :: Int | LayoutProps )
type AnnuaireKeyLayoutProps = ( key :: String | AnnuaireLayoutProps )


contactLayout :: Record AnnuaireLayoutProps -> R.Element
contactLayout props = R.createElement contactLayoutCpt props []

contactLayoutCpt :: R.Component AnnuaireLayoutProps
contactLayoutCpt = R.hooksComponentWithModule thisModule "contactLayout" cpt
  where
    cpt { annuaireId, appReload, asyncTasksRef, frontends, nodeId, session, treeReloadRef } _ = do
      let sid = sessionId session

      pure $ contactLayoutWithKey { annuaireId,
          appReload
        , asyncTasksRef
        , frontends
        , key: show sid <> "-" <> show nodeId
        , nodeId
        , session
        , treeReloadRef
        }

contactLayoutWithKey :: Record AnnuaireKeyLayoutProps -> R.Element
contactLayoutWithKey props = R.createElement contactLayoutWithKeyCpt props []

contactLayoutWithKeyCpt :: R.Component AnnuaireKeyLayoutProps
contactLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "contactLayoutWithKey" cpt
  where
    cpt { annuaireId, appReload, asyncTasksRef, frontends, nodeId, session, treeReloadRef } _ = do
      reload <- GUR.new

      cacheState <- R.useState' LT.CacheOn

      sidePanelTriggers <- LT.emptySidePanelTriggers

      useLoader nodeId (getAnnuaireContact session annuaireId) $
        \contactData@{contactNode: Contact' {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" }
               [ display { title: fromMaybe "no name" name }
                         (contactInfos hyperdata (onUpdateHyperdata reload))
               , Tabs.tabs {
                      appReload
                    , asyncTasksRef
                    , cacheState
                    , contactData
                    , frontends
                    , nodeId
                    , session
                    , sidePanelTriggers
                    , treeReloadRef
                    }
               ]
      where
        onUpdateHyperdata :: GUR.ReloadS -> HyperdataContact -> Effect Unit
        onUpdateHyperdata reload hd = do
          launchAff_ $ do
            _ <- saveContactHyperdata session nodeId hd
            liftEffect $ GUR.bump reload



getAnnuaireContact :: Session -> Int -> Int -> Aff ContactData'
getAnnuaireContact session annuaireId id = do
  contactNode :: Contact' <- get session $ Routes.NodeAPI Annuaire (Just annuaireId) $ show id
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure {contactNode, defaultListId: 424242}