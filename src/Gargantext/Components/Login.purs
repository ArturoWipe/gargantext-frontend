-- The Login component is a modal which allows the user to:
  -- See the current login session
  -- Select a backend and log into it
module Gargantext.Components.Login where

import Prelude (Unit, bind, const, discard, pure, ($), (<>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H

------------------------------------------------------------------------
import Gargantext.Components.Forms (clearfix, card, cardBlock, cardGroup, center, formGroup)
import Gargantext.Components.Login.Types (AuthRequest(..))
import Gargantext.Ends (Backend)
import Gargantext.Sessions (Session, Sessions, postAuthRequest, unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils (csrfMiddlewareToken)
import Gargantext.Utils.Reactix as R2

-- TODO: ask for login (modal) or account creation after 15 mn when user
-- is not logged and has made one search at least

type Props =
  ( backends :: Array Backend
  , sessions :: R2.Reductor Sessions Sessions.Action
  , setVisible :: R2.Setter Boolean )

type ModalProps = ( visible :: Boolean )

modal :: Record ModalProps -> Array R.Element -> R.Element
modal = R.createElement modalCpt

modalCpt :: R.Component ModalProps
modalCpt = R.staticComponent "Modal" cpt where
  cpt {visible} children =
    H.div { id: "loginModal", className: modalClass visible
          , role: "dialog", "data": {show: true}}
    [ H.div { className: "modal-dialog", role: "document"}
      [ H.div { className: "modal-content" }
        [ H.div { className: "modal-header" }
          [ H.h5 { className: "modal-title" } []
          , H.button { "type": "button", className: "close"
                     , "data": { dismiss: "modal" } }
            [ H.span { aria: { hidden: true } } [ H.text "X" ] ]
          , H.div { className: "modal-body" } children ] ] ] ]
  modalClass s = "modal myModal" <> if s then "" else " fade"

login :: Record Props -> R.Element
login props = R.createElement loginCpt props []

loginCpt :: R.Component Props
loginCpt = R.hooksComponent "G.C.Login.login" cpt
  where
    cpt props@{backends, sessions, setVisible} _ = do
      backend <- R.useState' Nothing
      pure $ case (fst backend) of
        Nothing -> chooser { backends, backend, sessions, setVisible }
        Just b -> form { sessions, setVisible, backend: b }

type ChooserProps = ( backend :: R.State (Maybe Backend) | Props )

chooser :: Record ChooserProps -> R.Element
chooser props = R.createElement chooserCpt props []

chooserCpt :: R.Component ChooserProps
chooserCpt = R.staticComponent "G.C.Login.chooser" cpt where
  cpt :: Record ChooserProps -> Array R.Element -> R.Element
  cpt {backend, backends, sessions, setVisible} _ =
    R.fragment
    [ renderSessions sessions, renderBackends backends backend ]
  
renderSessions :: R2.Reductor Sessions Sessions.Action -> R.Element
renderSessions sessions = render (unSessions $ fst sessions) where
  render Nothing = R.fragment []
  render (Just s) = renderSession s

renderSession :: Session -> R.Element
renderSession session = R.fragment []

renderBackends :: Array Backend -> R.State (Maybe Backend) -> R.Element
renderBackends backends state = R.fragment []

renderBackend :: Backend -> R.State (Maybe Backend) -> R.Element
renderBackend backend state = R.fragment []

type FormProps =
  ( backend :: Backend
  , sessions :: R2.Reductor Sessions Sessions.Action
  , setVisible :: R2.Setter Boolean )

form :: Record FormProps -> R.Element
form props = R.createElement formCpt props []

formCpt :: R.Component FormProps
formCpt = R.hooksComponent "G.C.Login.form" cpt where
  cpt :: Record FormProps -> Array R.Element -> R.Hooks R.Element
  cpt props@{backend, sessions, setVisible} _ = do
    error <- R.useState' ""
    username <- R.useState' ""
    password <- R.useState' ""
    pure $ H.div {className: "row"}
      [ logo
      , cardGroup
        [ card
          [ cardBlock
            [ center
              [ H.h4 {className: "m-b-0"}
                [ H.span {className: "icon-text"} [ H.text "Welcome :)" ] ]
              , H.p {className: "text-muted"}
                [ H.text $ "Login to your account or", requestAccessLink {} ] ]
            , H.div {}
              [ csrfTokenInput {}
              , formGroup [ H.p {} [ H.text (fst error) ], usernameInput username ]
              , formGroup [ passwordInput password, clearfix [] ]
              , center
                [ H.label {}
                  [ H.div {className: "checkbox"}
                    [ termsCheckbox {}, H.text "I accept the terms of use ", termsLink {} ] ]
                , loginSubmit $
                    onClick props error username password ] ] ] ] ] ]
  onClick {backend, sessions, setVisible} error username password e =
    launchAff_ $ do
      let req = AuthRequest {username: fst username, password: fst password}
      res <- postAuthRequest backend req
      case res of
        Left message -> liftEffect $ (snd error) (const message)
        Right sess -> liftEffect $ do
          (snd sessions) (Sessions.Login sess)
          (snd error) (const "")
          setVisible (const false)
  logo =
    H.div {className: "col-md-10 col-md-push-1"}
        [ H.h2 {className: "text-primary center m-a-2"}
    [ H.i {className: "material-icons md-36"} [ H.text "control_point" ]
    , H.span {className: "icon-text"} [ H.text "Gargantext" ] ] ]

csrfTokenInput :: {} -> R.Element
csrfTokenInput _ =
  H.input { type: "hidden", name: "csrfmiddlewaretoken"
          , value: csrfMiddlewareToken }-- TODO hard-coded CSRF token

termsCheckbox :: {} -> R.Element
termsCheckbox _ =
  H.input { id: "terms-accept", type: "checkbox", value: "", className: "checkbox" }

termsLink :: {} -> R.Element
termsLink _ =
  H.a { target: "_blank", href: termsUrl } [ H.text " [ Read the terms of use ] " ]
  where termsUrl = "http://gitlab.iscpif.fr/humanities/tofu/tree/master"

requestAccessLink :: {} -> R.Element
requestAccessLink _ =
  H.a { target: "_blank", href: applyUrl } [ H.text " request access" ]
  where applyUrl = "https://iscpif.fr/services/applyforourservices/"

usernameInput :: R.State String -> R.Element
usernameInput username =
  H.input { className: "form-control"
          , id: "id_username"
          , maxLength: "254"
          , name: "username"
          , placeholder: "username"
          , type: "text"
          , defaultValue: (fst username)
          --, on: {input: \e -> dispatch (SetUserName $ R2.unsafeEventValue e)}
          , on: {change: \e -> (snd username) $ const $ R2.unsafeEventValue e} }
 
passwordInput :: R.State String -> R.Element
passwordInput password =
  H.input { className: "form-control"
          , id: "id_password"
          , name: "password"
          , placeholder: "password"
          , type: "password"
          , defaultValue: (fst password)
          --, on: {input: \e -> dispatch (SetPassword $ R2.unsafeEventValue e)}
          , on: {change: \e -> (snd password) $ const $ R2.unsafeEventValue e} }

loginSubmit :: forall e. (e -> Effect Unit) -> R.Element
loginSubmit click =
  H.button { id, className, type: "submit", on: {click} } [ H.text "Login" ]
  where
    id = "login-button"
    className = "btn btn-primary btn-rounded"

