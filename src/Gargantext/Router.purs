module Gargantext.Router where

import Gargantext.Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Routing.Match (Match, lit, num)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

data Routes
  = Home
  | Login
  | SearchView
  | Folder             Int
    | Corpus           Int
      | AddCorpus
      | Document       Int Int
      | CorpusDocument Int Int Int
      | PGraphExplorer Int
      | Dashboard
    | Annuaire         Int
      | UserPage       Int
      | ContactPage       Int

routing :: Match Routes
routing = oneOf
  [ Login            <$   route "login"
  , SearchView       <$   route "search"
  , AddCorpus        <$   route "addCorpus"
  , Folder           <$> (route "folder"     *> int)
  , CorpusDocument   <$> (route "corpus" *> int) <*> (lit "list" *> int) <*> (lit "document" *> int)
  , Corpus           <$> (route "corpus"     *> int)
     , Document       <$> (route "list" *> int) <*> (lit "document" *> int)
     , Dashboard      <$   route "dashboard"
     , PGraphExplorer <$> (route "graph"      *> int)
  , Annuaire         <$> (route "annuaire"   *> int)
    , UserPage          <$> (route "user"    *> int)
    , ContactPage       <$> (route "contact" *> int)
  , Home             <$   lit ""
  ]

 where
    route str      = lit "" *> lit str

    int :: Match Int
    int = floor <$> num

instance showRoutes :: Show Routes where
  show Login            = "Login"
  show AddCorpus        = "AddCorpus"
  show SearchView       = "Search"
  show (UserPage i)     = "User"     <> show i
  show (ContactPage i)  = "Contact"  <> show i
  show (CorpusDocument _ _ i)     = "Document" <> show i
  show (Document _ i)     = "Document" <> show i
  show (Corpus i)       = "Corpus"   <> show i
  show (Annuaire i)     = "Annuaire" <> show i
  show (Folder   i)     = "Folder"   <> show i
  show Dashboard        = "Dashboard"
  show (PGraphExplorer i) = "graphExplorer" <> show i
  show Home             = "Home"
