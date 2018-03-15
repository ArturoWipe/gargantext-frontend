module Landing where

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, button, div, footer, h1, h3, hr, i, img, li, p, span, text, ul)
import React.DOM.Props (Props, _data, _id, aria, className, href, onClick, role, src, style, tabIndex, target, title)
import React (ReactElement)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, simpleSpec)
import Thermite as T

newtype State = State
  { userName :: String
  , password :: String
  }


initialState :: State
initialState = State
  {userName : ""
 , password : ""
  }

data Action
  = NoOp
  | Documentation
  | Enter
  | Login
  | SignUp


performAction :: forall eff props. PerformAction (console :: CONSOLE, ajax :: AJAX,dom::DOM | eff) State props Action
performAction NoOp _ _ = void do
  T.modifyState \state -> state

performAction Documentation _ _ = void do
  T.modifyState \state -> state

performAction Enter _ _ = void do
  lift $ setHash "/search"
  T.modifyState \state -> state

performAction Login _ _ = void do
  lift $ setHash "/login"
  T.modifyState \state -> state

performAction SignUp _ _ = void do
  T.modifyState \state -> state

jumboTitle :: Boolean -> ReactElement
jumboTitle b = div jumbo
                   [ div [className "row"             ]
                     [ div [className "col-md-8 content"]
                       [ h1 [] [ text "Gargantext"]
                       , p  [] [ text "search map share" ]
                       , p  [] [ a [ className "btn btn-success btn-lg spacing-class"
                                   , href "https://iscpif.fr/gargantext/your-first-map/"
                                   , target "blank"
                                   , title "Your first map in less than 5 minutes" 
                                   ]
                                   [ span [ aria {hidden : true}
                                          , className "glyphicon glyphicon-hand-right" 
                                          ]  []
                                   , text " Documentation"
                                   ]
                                ]
                       ]
                     , div [ className "col-md-2 content"]
                           [p [ className "right" ]
                              [ div [_id "logo-designed" ]
                              [ img [ src "images/logo.png", title "Project hosted by CNRS (France, Europa, Solar System)" ]
                              []
                           ]
                         ]
                       ]
                     ]
                   ]
                  where
                    jumbo = case b of
                                 true  -> [className "jumbotron"]
                                 false -> []


imageEnter :: Props -> ReactElement
imageEnter action =  div [className "row"]
                           [ div [className "col-md-offset-5 col-md-6 content"]
                             [ img [ src "images/Gargantextuel-212x300.jpg"
                                   , _id "funnyimg"
                                   , title "Click and test by yourself"
                                   , action
                                   ] 
                                   []
                             ]
                           ]


home :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
home = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ div [ className "container" ] [ jumboTitle true                            ]
      , div [ className "container" ] [ imageEnter (onClick \_ -> dispatch $ Enter)]
      , div [ className "container" ] [ blocksRandomText                          ]
      ]

blocksRandomText :: ReactElement
blocksRandomText = div [ className "row" ]
          [ div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Random sentences in Gargantua's Books chapters, historically true" ]
              -- TODO click on icon and randomize the text below
                [ i [className "fas fa-random"] []
                , text "   Historic" 
                ]
              ]
            , p []
            -- TODO use RandomText.randomSentences on this text (should be editable by user later)
              [ text "Chapter 1.XV. How Gargantua was put under other schoolmasters. Chapter 2.XXII. How Panurge served a Parisian lady a trick that pleased her not very well. Chapter 3.XXXVII. How Pantagruel persuaded Panurge to take counsel of a fool. Chapter 4.LXI. How Gaster invented means to get and preserve corn. Chapter 5.XXXVIII. Of the temple's admirable pavement." ]
            ]
          , div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Randomized words, semantically and syntaxically falses." ]
              -- TODO click on icon and randomize the text below
                [ i [className "fas fa-random"] []
                , text "   Presentation" 
                ]
              ]
            , p []
            -- TODO use RandomText.randomWords on this text (should be editable by user later)
              [ text "Autem nascetur iaculis, sedfusce enimsed cursus posuere consectetuer eu justo aliquammauris. Phasellus vero nisi porttitor elit quod, leo feliscras ultricies non tempor sagittis. Liberoduis facilisinam erat dapibusnam, lacus dui duis tristique volutpatut quis vestibulum magna. Nobis faucibusvestibulum dolores minim. Bibendumin malesuada adipiscing ante, mattis fames nequeetiam lorem. No diam id. Litora quisaenean commodo lobortisetiam neque, libero mollis scelerisque inceptos ullamcorper sea congue delenit possim.            " ]
            ]
          , div [ className "col-md-4 content" ]
            [ h3 []
              [ a [ href "#", title "Randomized letters, true or false ?" ]
              -- TODO click on icon and randomize the text below
                [ i [className "fas fa-random"] []
                , text "   Tutoreil" 
                ]
              ]
            , p []
            -- TODO use RandomText.randomChars on this text (should be editable by user later)
              [ text "Il paraît que l'rdore des lettres dans un mot n'a pas d'imtraopnce. La première et la dernière lettre doeivnt être à la bonne place. Le reste peut être dans un désordre total et on peut touojurs lire sans prolèbme. On ne lit donc pas chaque lettre en ellêem-me, mais le mot comme un tout. Un chaegmnent de référentiel et nous tranpossons ce résultat au texte luimê-me: l'rdore des mots est failbement important copamré au contexte du texte qui, lui, est copmté: comptexter avec Gargantext.                "
              , text " "
              , text ""
              ]
            ]
          ]



