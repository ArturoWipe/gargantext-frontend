module Gargantext.Components.Bootstrap.BaseModal (baseModal) where

import Gargantext.Prelude

import Effect (Effect)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( isVisibleBox :: T.Box Boolean
  | Options
  )

type Options =
  ( id :: String
  )

options :: Record Options
options =
  { id: ""
  }

cname :: String
cname = "b-modal"

baseModal :: forall r. R2.OptComponent Options Props r
baseModal = R2.optComponent component options

component :: R.Component Props
component = R.hooksComponent cname cpt where
  cpt { isVisibleBox
      , id
      } children = do

    isVisible <- R2.useLive' isVisibleBox

    R.createPortal
      [
        H.div
        { id
        , className: cname
        , role: "dialog"
        , data: { show: true }
        , style: { display: isVisible ? "block" $ "none" }
        }
        [ H.div { className: "modal-dialog modal-lg", role: "document"}
          [ H.div { className: "modal-content" }
            [ H.div { className: "modal-header" }
              [ H.div { className: "col-md-10 col-md-push-1" }
                [ H.h2 { className: "text-primary center m-a-2" }
                  [
                    H.span {className: "center icon-text"}
                    [ H.text "Add a new document" ]
                  ]
                ]
              , H.button
                { type: "button", className: "close"
                , data: { dismiss: "modal" }
                }
                [
                  H.a
                  { on: { click: toggle }
                  , className: "btn fa fa-times" }
                  []
                ]
              ]
            , H.div
              { className: "modal-body" }
              children
            ]
          ]
        ]
      ]
      <$> R2.getPortalHost


toggle :: forall event. event -> T.Box Boolean -> Effect Unit
toggle _ = T.modify_ not
