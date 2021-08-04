module Gargantext.Components.Table where

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.FolderView as FV
import Gargantext.Components.Table.Types (ColumnName, OrderBy, OrderByDirection(..), Params, Props, TableContainerProps, columnName)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Search (SearchType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reactix (effectLink)

here :: R2.Here
here = R2.here "Gargantext.Components.Table"

type Page = Int

type State =
  { page       :: Page
  , pageSize   :: PageSizes
  , orderBy    :: OrderBy
  , searchType :: SearchType
  }

paramsState :: Params -> State
paramsState {offset, limit, orderBy, searchType} = {pageSize, page, orderBy, searchType}
  where
    pageSize = int2PageSizes limit
    page = offset / limit + 1

stateParams :: State -> Params
stateParams {pageSize, page, orderBy, searchType} = {offset, limit, orderBy, searchType}
  where
    limit = pageSizes2Int pageSize
    offset = limit * (page - 1)

type TableHeaderLayoutProps = (
    cacheState :: T.Box NT.CacheState
  , date  :: String
  , desc  :: String
  , key   :: String
  , query :: String
  , title :: String
  , user  :: String
  )

initialParams :: Params
initialParams = stateParams {page: 1, pageSize: PS10, orderBy: Nothing, searchType: SearchDoc}
-- TODO: Not sure this is the right place for this

tableHeaderLayout :: R2.Component TableHeaderLayoutProps
tableHeaderLayout = R.createElement tableHeaderLayoutCpt
tableHeaderLayoutCpt :: R.Component TableHeaderLayoutProps
tableHeaderLayoutCpt = here.component "tableHeaderLayout" cpt
  where
    cpt { cacheState, date, desc, query, title, user } _ = do
      cacheState' <- T.useLive T.unequal cacheState

      pure $ R.fragment
        [ R2.row [FV.backButton]
        ,
          R2.row
          [ H.div {className: "col-md-3"} [ H.h3 {} [H.text title] ]
          , H.div {className: "col-md-9"}
            [ H.hr {style: {height: "2px", backgroundColor: "black"}} ]
          ]
          , R2.row
            [ H.div {className: "col-md-8 content"}
              [ H.p {}
                [ H.span {className: "fa fa-globe"} []
                , H.text $ " " <> desc
                ]
              , H.p {}
                [ H.span {className: "fa fa-search-plus"} []
                , H.text $ " " <> query
                ]
              , H.p { className: "cache-toggle"
                    , on: { click: cacheClick cacheState } }
                [ H.span { className: "fa " <> (cacheToggle cacheState') } []
                , H.text $ cacheText cacheState'
                ]
              ]
            , H.div {className: "col-md-4 content"}
              [ H.p {}
                [ H.span {className: "fa fa-calendar"} []
                , H.text $ " " <> date
                ]
              , H.p {}
                [ H.span {className: "fa fa-user"} []
                , H.text $ " " <> user
                ]
              ]
            ]
          ]

    cacheToggle NT.CacheOn = "fa-toggle-on"
    cacheToggle NT.CacheOff = "fa-toggle-off"

    cacheText NT.CacheOn = "Cache On"
    cacheText NT.CacheOff = "Cache Off"

    cacheClick cacheState _ = do
      T.modify cacheStateToggle cacheState

    cacheStateToggle NT.CacheOn = NT.CacheOff
    cacheStateToggle NT.CacheOff = NT.CacheOn

table :: Record Props -> R.Element
table props = R.createElement tableCpt props []
tableCpt :: R.Component Props
tableCpt = here.component "table" cpt
  where
    cpt { colNames
        , container
        , params
        , rows
        , syncResetButton
        , totalRecords
        , wrapColElts } _ = do
      params' <- T.useLive T.unequal params

      let
        state = paramsState params'
        ps = pageSizes2Int state.pageSize
        totalPages = (totalRecords / ps) + min 1 (totalRecords `mod` ps)
        colHeader :: ColumnName -> R.Element
        colHeader c = H.th {scope: "col"} [ H.b {} cs ]
          where
            lnk mc = effectLink $ void $ T.modify (_ { orderBy = mc }) params
            cs :: Array R.Element
            cs =
              wrapColElts c $
              case state.orderBy of
                Just (ASC d)  | c == d -> [lnk (Just (DESC c)) "ASC " , lnk Nothing (columnName c)]
                Just (DESC d) | c == d -> [lnk (Just (ASC  c)) "DESC ", lnk Nothing (columnName c)]
                _ -> [lnk (Just (ASC c)) (columnName c)]
      pure $ container
        { pageSizeControl: sizeDD { params }
        , pageSizeDescription: textDescription state.page state.pageSize totalRecords
        , paginationLinks: pagination { params, totalPages }
        , syncResetButton
        , tableBody: map _.row $ A.fromFoldable rows
        , tableHead: H.tr {} (colHeader <$> colNames)
        }

makeRow :: Array R.Element -> R.Element
makeRow els = H.tr {} $ (\c -> H.td {} [c]) <$> els


type FilterRowsParams =
  (
    params :: Params
  )

filterRows :: forall a. Record FilterRowsParams -> Seq.Seq a -> Seq.Seq a
filterRows { params: { limit, offset, orderBy } } rs = newRs
  where
    newRs = Seq.take limit $ Seq.drop offset $ rs

defaultContainer :: {title :: String} -> Record TableContainerProps -> R.Element
defaultContainer {title} props = R.fragment $ props.syncResetButton <> controls
  where
    controls = [ R2.row
                 [ H.div {className: "col-md-4"} [ props.pageSizeDescription ]
                 , H.div {className: "col-md-4"} [ props.paginationLinks ]
                 , H.div {className: "col-md-4"} [ props.pageSizeControl ]
                 ]
               , R2.row [
                   H.table {className: "col-md-12 table"}
                   [ H.thead {className: ""} [ props.tableHead ]
                   , H.tbody {} props.tableBody
                   ]
                 ]
               ]

-- TODO: this needs to be in Gargantext.Pages.Corpus.Graph.Tabs
graphContainer :: {title :: String} -> Record TableContainerProps -> R.Element
graphContainer {title} props =
  -- TODO title in tabs name (above)
  H.table {className: "table"}
  [ H.thead {className: ""} [ props.tableHead ]
  , H.tbody {} props.tableBody
  ]
   -- TODO better rendering of the paginationLinks
   -- , props.pageSizeControl
   -- , props.pageSizeDescription
   -- , props.paginationLinks

type SizeDDProps =
  (
    params :: T.Box Params
  )

sizeDD :: Record SizeDDProps -> R.Element
sizeDD p = R.createElement sizeDDCpt p []

sizeDDCpt :: R.Component SizeDDProps
sizeDDCpt = here.component "sizeDD" cpt
  where
    cpt { params } _ = do
      params' <- T.useLive T.unequal params
      let { pageSize } = paramsState params'

      pure $ H.span {} [
        R2.select { className, defaultValue: show pageSize, on: {change} } sizes
      ]
      where
        className = "form-control"
        change e = do
          let ps = string2PageSize $ R.unsafeEventValue e
          T.modify (\p -> stateParams $ (paramsState p) { pageSize = ps }) params
        sizes = map option pageSizes
        option size = H.option {value} [H.text value]
          where value = show size

textDescription :: Int -> PageSizes -> Int -> R.Element
textDescription currPage pageSize totalRecords =
  H.div {className: "row1"} [ H.div {className: ""} [ H.text msg ] ] -- TODO or col-md-6 ?
  where
    start = (currPage - 1) * pageSizes2Int pageSize + 1
    end' = currPage * pageSizes2Int pageSize
    end  = if end' > totalRecords then totalRecords else end'
    msg = "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords

changePage :: Page -> T.Box Params -> Effect Unit
changePage page params =
  void $ T.modify (\p -> stateParams $ (paramsState p) { page = page }) params

type PaginationProps =
  ( params     :: T.Box Params
  , totalPages :: Int )

pagination :: R2.Leaf PaginationProps
pagination props = R.createElement paginationCpt props []

paginationCpt :: R.Component PaginationProps
paginationCpt = here.component "pagination" cpt
  where
    cpt { params, totalPages } _ = do
      params' <- T.useLive T.unequal params
      let { page } = paramsState params'
          prev = if page == 1 then
                  H.text " Prev. "
                else
                  changePageLink (page - 1) "Prev."
          next = if page == totalPages then
                  H.text " Next "
                else
                  changePageLink (page + 1) "Next"
          first = if page == 1 then
                    H.text ""
                  else
                    changePageLink' 1
          last = if page == totalPages then
                  H.text ""
                else
                  changePageLink' totalPages
          ldots = if page >= 5 then
                    H.text " ... "
                    else
                    H.text ""
          rdots = if page + 3 < totalPages then
                    H.text " ... "
                    else
                    H.text ""
          lnums = map changePageLink' $ A.filter (1  < _) [page - 2, page - 1]
          rnums = map changePageLink' $ A.filter (totalPages > _) [page + 1, page + 2]


      pure $ H.span {} $
        [ H.text " ", prev, first, ldots]
        <>
        lnums
        <>
        [H.b {} [H.text $ " " <> show page <> " "]]
        <>
        rnums
        <>
        [ rdots, last, next ]
        where
          changePageLink :: Int -> String -> R.Element
          changePageLink i s =
            H.span {}
              [ H.text " "
              , effectLink (changePage i params) s
              , H.text " "
              ]

          changePageLink' :: Int -> R.Element
          changePageLink' i = changePageLink i (show i)

data PageSizes = PS10 | PS20 | PS50 | PS100 | PS200

derive instance Eq PageSizes

instance Show PageSizes where
  show PS10  = "10"
  show PS20  = "20"
  show PS50  = "50"
  show PS100 = "100"
  show PS200 = "200"

int2PageSizes :: Int -> PageSizes
int2PageSizes i = string2PageSize $ show i

pageSizes2Int :: PageSizes -> Int
pageSizes2Int PS10  = 10
pageSizes2Int PS20  = 20
pageSizes2Int PS50  = 50
pageSizes2Int PS100 = 100
pageSizes2Int PS200 = 200

pageSizes :: Array PageSizes
pageSizes = [PS10, PS20, PS50, PS100, PS200]

string2PageSize :: String -> PageSizes
string2PageSize "10" = PS10
string2PageSize "20" = PS20
string2PageSize "50" = PS50
string2PageSize "100" = PS100
string2PageSize "200" = PS200
string2PageSize _    = PS10
