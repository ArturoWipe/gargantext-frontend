module Gargantext.Components.Forest.Tree.Node.Action.Search.Frame where

{-
import Data.Array as A
import Data.Map as Map
import Data.String as S
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), FileType(..), UploadFileContents(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup(..), addNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.CopyFrom (copyFromCorpusView)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameAction)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar (searchBar)
import Gargantext.Components.Forest.Tree.Node.Action.Share as Share
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), uploadFileView, fileTypeView, uploadTermListView)
import Gargantext.Components.Forest.Tree.Node.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.Forest.Tree.Node.Tools
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (allLangs, Lang(EN))
import Gargantext.Components.NgramsTable.API as NTAPI
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Ends (Frontends, url)
-}

import DOM.Simple as DOM
import DOM.Simple.Event (MessageEvent)
import DOM.Simple.EventListener (Callback, addEventListener, callback)
import DOM.Simple.Window (window)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (Search, isIsTex_Advanced)
import Gargantext.Components.Forest.Tree.Node.Action.Search.Types (DataField(..))
import Gargantext.Components.Forest.Tree.Node.Box.Types
import Gargantext.Prelude (discard, identity, pure, unit, ($), (<>), (==))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as NQP
import URI.Query as Query

--------------------
-- | Iframes
searchIframes :: Record NodePopupProps
              -> R.State Search
              -> R.Ref (Nullable DOM.Element)
              -> R.Element
searchIframes {nodeType} search@(search' /\ _) iframeRef =
  if isIsTex_Advanced search'.datafield then
    H.div { className: "istex-search panel panel-default" }
          [ iframeWith "https://istex.gargantext.org" search iframeRef ]
  else
    if Just Web == search'.datafield then
      H.div { className: "istex-search panel panel-default" }
            [ iframeWith "https://searx.gargantext.org" search iframeRef ]
    else
      H.div {} []

iframeWith :: String
           -> R.State Search
           -> R.Ref (Nullable DOM.Element)
           -> R.Element
iframeWith url (search /\ setSearch) iframeRef =
  H.iframe { src: isTexTermUrl search.term
            ,width: "100%"
            ,height: "100%"
            ,ref: iframeRef
            ,on: {
              load: \_ -> do
                 addEventListener window "message" (changeSearchOnMessage url)
                 R2.postMessage iframeRef search.term
                 }
           } []
  where
    changeSearchOnMessage :: String -> Callback MessageEvent
    changeSearchOnMessage url' =
      callback $ \m -> if R2.getMessageOrigin m == url'
                         then do
                           let {url'', term} = R2.getMessageData m
                           setSearch $ _ {url = url'', term = term}
                         else
                           pure unit
    isTexTermUrl term = url <> query
      where
        query = Query.print $ NQP.print identity identity qp

        qp = NQP.QueryPairs [
          Tuple (NQP.keyFromString "query") (Just (NQP.valueFromString term))
          ]

