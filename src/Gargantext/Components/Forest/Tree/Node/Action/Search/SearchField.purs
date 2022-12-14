module Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField where

import Gargantext.Prelude

import DOM.Simple.Console (log, log2)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (over)
import Data.Nullable (null)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Action.Search.Frame (searchIframes)
import Gargantext.Components.Forest.Tree.Node.Action.Search.Types (DataField(..), Database(..), IMT_org(..), Org(..), SearchQuery(..), allIMTorgs, allOrgs, dataFields, defaultSearchQuery, doc, performSearch, datafield2database, Search)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Lang (Lang)
import Gargantext.Components.ListSelection as ListSelection
import Gargantext.Components.ListSelection.Types as ListSelection
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Sessions (Session)
import Gargantext.Types (FrontendError)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField"

defaultSearch :: Search
defaultSearch = { databases: Empty
                , datafield: Nothing
                , node_id  : Nothing
                , lang     : Nothing
                , term     : ""
                , url      : ""
                }

type Props =
  -- list of databases to search, or parsers to use on uploads
  ( databases :: Array Database
  , errors    :: T.Box (Array FrontendError)
  , langs     :: Array Lang
  -- State hook for a search, how we get data in and out
  , onSearch  :: GT.AsyncTaskWithType -> Effect Unit
  , search    :: T.Box Search
  , session   :: Session
  )

searchField :: R2.Component Props
searchField = R.createElement searchFieldCpt
searchFieldCpt :: R.Component Props
searchFieldCpt = here.component "searchField" cpt
  where
    cpt { databases, errors, langs, onSearch, search, session } _ = do
      selection <- T.useBox ListSelection.MyListsFirst

      pure $
        H.div { className: "search-field" }
        [ searchInput { search } []
          -- , if length s.term < 3  -- search with love : <3
          --   then
          --     H.div {}[]
          --   else
        , datafieldInput { databases, langs, search } []
        , ListSelection.selection { selection, session } []
        , submitButton { errors, onSearch, search, selection, session } []
        ]
      --pure $ panel params button

type ComponentProps =
  ( search :: T.Box Search )

componentIMT :: R2.Component ComponentProps
componentIMT = R.createElement componentIMTCpt
componentIMTCpt :: R.Component ComponentProps
componentIMTCpt = here.component "componentIMT" cpt
  where
    cpt { search } _ = do
      search' <- T.useLive T.unequal search

      let liCpt org =
            H.li {}
            [ H.input { type: "checkbox"
                      , checked: isIn org search'.datafield
                      , on: { change: \_ -> ( T.modify_ (_ { datafield = updateFilter org search'.datafield }) search)
                            }
                      }
            , if org == All_IMT
              then H.i {} [H.text  $ " " <> show org]
              else H.text $ " " <> show org
            ]

      pure $ R.fragment
        [ H.ul {} $ map liCpt allIMTorgs
        --, filterInput fi
        ]

componentCNRS :: R2.Component ComponentProps
componentCNRS = R.createElement componentCNRSCpt
componentCNRSCpt :: R.Component ComponentProps
componentCNRSCpt = here.component "componentCNRS" cpt
  where
    cpt _ _ = do
      pure $ R.fragment
        [ H.div {} []
          --, filterInput fi
        ]


isExternal :: Maybe DataField -> Boolean
isExternal (Just (External _)) = true
isExternal _ = false

isHAL :: Maybe DataField -> Boolean
isHAL (Just
        ( External
          ( Just (HAL _ )
          )
        )
      ) = true
isHAL _ = false

isIsTex :: Maybe DataField -> Boolean
isIsTex ( Just
          ( External
            ( Just ( IsTex)
            )
          )
        ) = true
isIsTex _ = false


isIMT :: Maybe DataField -> Boolean
isIMT ( Just
        ( External
          ( Just
            ( HAL
              ( Just ( IMT _)
              )
            )
          )
        )
      ) = true
isIMT _ = false

isCNRS :: Maybe DataField -> Boolean
isCNRS ( Just
         ( External
          ( Just
            ( HAL
              ( Just ( CNRS _)
              )
            )
          )
        )
      ) = true
isCNRS _ = false

needsLang :: Maybe DataField -> Boolean
needsLang (Just Gargantext) = true
needsLang (Just Web)        = true
needsLang ( Just
            ( External
              ( Just (HAL _)
              )
            )
          ) = true
needsLang _                 = false


isIn :: IMT_org -> Maybe DataField -> Boolean
isIn org ( Just
           ( External
             ( Just
               ( HAL
                 ( Just
                   ( IMT imtOrgs )
                 )
               )
             )
           )
         ) = Set.member org imtOrgs
isIn _ _ = false

updateFilter :: IMT_org -> Maybe DataField -> Maybe DataField
updateFilter org (Just (External (Just (HAL (Just (IMT imtOrgs)))))) =
 (Just (External (Just (HAL (Just $ IMT imtOrgs')))))
  where
    imtOrgs' = if Set.member org imtOrgs
                  then
                    if org == All_IMT
                       then Set.empty
                       else Set.delete All_IMT $ Set.delete org imtOrgs
                  else
                    if org == All_IMT
                       then Set.fromFoldable allIMTorgs
                       else Set.insert org imtOrgs

updateFilter org _ = (Just (External (Just (HAL (Just (IMT imtOrgs'))))))
  where
    imtOrgs' = if org == All_IMT
                  then Set.fromFoldable allIMTorgs
                  else Set.fromFoldable [org]

------------------------------------------------------------------------

type LangNavProps =
  ( langs  :: Array Lang
  , search :: T.Box Search )

langNav :: R2.Component LangNavProps
langNav = R.createElement langNavCpt
langNavCpt :: R.Component LangNavProps
langNavCpt = here.component "langNav" cpt
  where
    cpt { langs, search } _ = do
      search' <- T.useLive T.unequal search

      pure $ R.fragment
        [ H.div {className: "text-primary center"} [H.text "with lang"]
        , H.div {className: "nav nav-tabs"} ((liItem search') <$> langs)
        ]
        where
          liItem :: Search -> Lang -> R.Element
          liItem { lang } lang' =
            H.div { className : "nav-item nav-link" <> if (Just lang') == lang then " active" else ""
                  , on: { click: \_ -> T.modify_ (_ { lang = Just lang' }) search }
                  }
            [ H.text (show lang') ]

------------------------------------------------------------------------

type DataFieldNavProps =
  ( search :: T.Box Search )

dataFieldNav :: R2.Component DataFieldNavProps
dataFieldNav = R.createElement dataFieldNavCpt
dataFieldNavCpt :: R.Component DataFieldNavProps
dataFieldNavCpt = here.component "dataFieldNav" cpt
  where
    cpt { search } _ = do
      search'@{ datafield } <- T.useLive T.unequal search

      pure $ R.fragment
        [ H.div { className: "text-primary center"} [H.text "with DataField"]
        , H.div { className: "nav nav-tabs" } ((liItem search') <$> dataFields)
        , H.div { className: "center" } [ H.text
                                        $ maybe "TODO: add Doc Instance" doc datafield
                                      ]
        ]
      where
        liItem :: Search -> DataField -> R.Element
        liItem { datafield } df' =
          H.div { className : "nav-item nav-link"
                            <> if isActive  --(Just df') == datafield
                                then " active"
                                else ""
              , on: { click: \_ -> T.modify_ (_ { datafield = Just df'
                                                , databases = datafield2database df'
                                                }) search
                    }
              -- just one database query for now
              -- a list a selected database needs more ergonomy
              } [ H.text (show df') ]
          where
            isActive = show (Just df') == show datafield

------------------------------------------------------------------------

type DatabaseInputProps = (
    databases :: Array Database
  , search    :: T.Box Search
  )

databaseInput :: R2.Component DatabaseInputProps
databaseInput = R.createElement databaseInputCpt
databaseInputCpt :: R.Component DatabaseInputProps
databaseInputCpt = here.component "databaseInput" cpt
  where
    cpt { databases
        , search } _ = do
      search' <- T.useLive T.unequal search

      let db = case search'.datafield of
            (Just (External (Just x))) -> Just x
            _                          -> Nothing

          liItem :: Database -> R.Element
          liItem  db' = H.option { className : "text-primary center"
                                 , value: show db' } [ H.text (show db') ]

          change e = do
            let value = read $ R.unsafeEventValue e
            T.modify_ (_ { datafield = Just $ External value
                         , databases = fromMaybe Empty value
                         }) search

      pure $
        H.div { className: "form-group" }
        [ H.div {className: "text-primary center"} [ H.text "in database" ]
        , R2.select { className: "form-control"
                    , defaultValue: defaultValue search'.datafield
                    , on: { change }
                    } (liItem <$> databases)
        , H.div {className:"center"} [ H.text $ maybe "" doc db ]
        ]

    defaultValue datafield = show $ maybe Empty datafield2database datafield


type OrgInputProps =
  ( orgs :: Array Org
  | ComponentProps)

orgInput :: R2.Component OrgInputProps
orgInput = R.createElement orgInputCpt
orgInputCpt :: R.Component OrgInputProps
orgInputCpt = here.component "orgInput" cpt
  where
    cpt { orgs, search } _ = do
      let change e = do
            let value = R.unsafeEventValue e
            T.modify_ (_ { datafield = Just $ External $ Just $ HAL $ read value }) search

      pure $ H.div { className: "form-group" }
        [ H.div {className: "text-primary center"} [H.text "filter with organization: "]
        , R2.select { className: "form-control"
                    , on: { change }
                    } (liItem <$> orgs)
        ]

    liItem :: Org -> R.Element
    liItem  org = H.option {className : "text-primary center"} [ H.text (show org) ]

{-
filterInput :: R.State String -> R.Element
filterInput (term /\ setTerm) =
  H.div { className: "form-group" }
        [ H.input { defaultValue: term
                  , className: "form-control"
                  , type: "text"
                  , on: { change: setTerm <<< const <<< R.unsafeEventValue }
                  , "required pattern": "[[0-9]+[ ]+]*"
                  -- TODO                          ^FIXME not sure about the regex comprehension: that should match "123 2334 44545" only (Integers separated by one space)
                  -- form validation with CSS
                  -- DOC: https://developer.mozilla.org/en-US/docs/Learn/HTML/Forms/Form_validation
                  , placeholder : "Filter with struct_Ids as integer"
                  }
         ]
-}

type DatafieldInputProps =
  ( databases :: Array Database
  , langs :: Array Lang
  , search :: T.Box Search )

datafieldInput :: R2.Component DatafieldInputProps
datafieldInput = R.createElement datafieldInputCpt
datafieldInputCpt :: R.Component DatafieldInputProps
datafieldInputCpt = here.component "datafieldInput" cpt where
  cpt { databases, langs, search } _ = do
    search' <- T.useLive T.unequal search
    iframeRef <- R.useRef null
    
    pure $ H.div {}
      [ dataFieldNav { search } []
        
      , if isExternal search'.datafield
        then databaseInput { databases, search } []
        else H.div {} []
             
      , if isHAL search'.datafield
        then orgInput { orgs: allOrgs, search } []
        else H.div {} []
             
      , if isIMT search'.datafield
        then componentIMT { search } []
        else H.div {} []
             
      , if isCNRS search'.datafield
        then componentCNRS { search } []
        else H.div {} []
             
      , if needsLang search'.datafield
        then langNav { langs, search } []
        else H.div {} []
             
      , H.div {} [ searchIframes { iframeRef, search } [] ]
      ]

type SearchInputProps =
  (
    search :: T.Box Search
  )

searchInput :: R2.Component SearchInputProps
searchInput = R.createElement searchInputCpt
searchInputCpt :: R.Component SearchInputProps
searchInputCpt = here.component "searchInput" cpt
  where
    cpt { search } _ = do
      { term } <- T.useLive T.unequal search
      valueRef <- R.useRef term

      pure $ H.div { className: "" }
        [ inputWithEnter { onBlur: onBlur valueRef search
                         , onEnter: onEnter valueRef search
                         , onValueChanged: onValueChanged valueRef
                         , autoFocus: false
                         , className: "form-control"
                         , defaultValue: R.readRef valueRef
                         , placeholder: "Your query here"
                         , type: "text" }
        ]

      -- pure $
      --   H.div { className : "" }
      --   [ H.input { className: "form-control"
      --             , defaultValue: search.term
      --             , on: { input : onInput valueRef setSearch }
      --             , placeholder: "Your Query here"
      --             , type: "text"
      --             }
      --   ]
    onBlur valueRef search value = do
      R.setRef valueRef value
      T.modify_ (_ { term = value }) search
    onEnter valueRef search _ = do
      T.modify_ (_ { term = R.readRef valueRef }) search

    onValueChanged valueRef value = do
      R.setRef valueRef value
      -- setSearch $ _ { term = value }

type SubmitButtonProps =
  ( errors    :: T.Box (Array FrontendError)
  , onSearch  :: GT.AsyncTaskWithType -> Effect Unit
  , search    :: T.Box Search
  , selection :: T.Box ListSelection.Selection
  , session   :: Session
  )

submitButton :: R2.Component SubmitButtonProps
submitButton = R.createElement submitButtonComponent
submitButtonComponent :: R.Component SubmitButtonProps
submitButtonComponent = here.component "submitButton" cpt
  where
    cpt { errors, onSearch, search, selection, session } _ = do
      search' <- T.useLive T.unequal search
      selection' <- T.useLive T.unequal selection

      pure $
        H.button { className: "btn btn-primary"
                 , "type"   : "button"
                 , on       : { click: doSearch onSearch errors session selection' search' }
                 , style    : { width: "100%" }
                 }
        [ H.text "Launch Search" ]

    doSearch onSearch errors session selection search = \_ -> do
      log2 "[submitButton] searching" search
      triggerSearch { onSearch, errors, session, selection, search }
      --case search.term of
      --  "" -> setSearch $ const defaultSearch
      --  _  -> setSearch $ const q

type TriggerSearch =
  ( errors    :: T.Box (Array FrontendError)
  , onSearch  :: GT.AsyncTaskWithType -> Effect Unit
  , search    :: T.Box Search
  , selection :: T.Box ListSelection.Selection
  , session   :: Session
  )

triggerSearch :: { onSearch :: (GT.AsyncTaskWithType -> Effect Unit)
                 , errors :: T.Box (Array FrontendError)
                 , session :: Session
                 , selection :: ListSelection.Selection
                 , search :: Search }
              -> Effect Unit
triggerSearch { onSearch, errors, session, selection, search } =
  launchAff_ $ do
    liftEffect $ do
      let here' = "[triggerSearch] Searching "
      log2 (here' <> "databases: ") (show search.databases)
      log2 (here' <> "datafield: ") (show search.datafield)
      log2 (here' <> "term: ")            search.term
      log2 (here' <> "lang: ")      (show search.lang)

    case search.node_id of
      Nothing -> liftEffect $ log "[triggerSearch] node_id is Nothing, don't know what to do"
      Just id -> do
        eTask <- performSearch session id $ searchQuery selection search
        handleRESTError errors eTask $ \task -> liftEffect $ do
          log2 "[triggerSearch] task" task
          onSearch task

    --liftEffect $ do
    --  log2 "Return:" r
    --  modalShow "addCorpus"

searchQuery :: ListSelection.Selection -> Search -> SearchQuery
searchQuery selection { datafield: Nothing, term } =
  over SearchQuery (_ { query = term
                      , selection = selection }) defaultSearchQuery
searchQuery selection { databases, datafield, lang, term, node_id } =
  over SearchQuery (_ { databases = databases
                      , datafield = datafield
                      , lang      = lang
                      , node_id   = node_id
                      , query     = term
                      , selection = selection
                      }) defaultSearchQuery
 
