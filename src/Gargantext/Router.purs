module Gargantext.Router where

import Prelude

import Control.Alt ((<|>))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Routing.Match (Match, lit, num)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

data Routes
  = Home
  | Login
  | AddCorpus
  | DocView        Int
  | SearchView
  | UserPage       Int
  | DocAnnotation  Int
  | Tabview
  | Corpus         Int
  | PGraphExplorer
  | NGramsTable
  | Dashboard
  | Annuaire       Int
  | Folder         Int


instance showRoutes :: Show Routes where
  show Login            = "Login"
  show AddCorpus        = "AddCorpus"
  show (DocView i)      = "DocView"
  show SearchView       = "Search"
  show (UserPage i)     = "User" <> show i
  show (DocAnnotation i)= "Document"
  show Tabview          = "Tabview"
  show PGraphExplorer   = "graphExplorer"
  show NGramsTable      = "NGramsTable"
  show Dashboard        = "Dashboard"
  show (Corpus i)       = "Corpus" <> show i
  show (Annuaire i)     = "Annuaire" <> show i
  show (Folder   i)     = "Folder"   <> show i
  show Home             = "Home"

int :: Match Int
int = floor <$> num

routing :: Match Routes
routing =
      Login          <$   route "login"
  <|> Tabview        <$   route "tabview"
  <|> DocAnnotation  <$> (route "document" *> int)
  <|> UserPage       <$> (route "user"     *> int)
  <|> SearchView     <$   route "search"
  <|> DocView        <$> (route "docView"  *> int)
  <|> AddCorpus      <$   route "addCorpus"
  <|> Corpus         <$> (route "corpus"   *> int)
  <|> PGraphExplorer <$   route "graph"
  <|> NGramsTable    <$   route "ngrams"
  <|> Dashboard      <$   route "dashboard"
  <|> Annuaire       <$> (route "annuaire" *> int)
  <|> Folder         <$> (route "folder"   *> int)
  <|> Home           <$   lit ""
  where
    route str      = lit "" *> lit str

routeHandler :: (Maybe Routes -> Routes -> Effect Unit) -> Maybe Routes -> Routes -> Effect Unit
routeHandler dispatchAction old new = do
  liftEffect $ log $ "change route : " <> show new
  w      <- window
  ls     <- localStorage w
  token  <- getItem "accessToken" ls
  let tkn = token
  liftEffect $ log $ "JWToken : " <> show tkn
  case tkn of
    Nothing -> do
      dispatchAction old new
      liftEffect $ log $ "called SignIn Route :"
    Just t -> do
      dispatchAction old new
      liftEffect $ log $ "called Route : " <> show new
