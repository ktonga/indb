{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module INDB.Netflix where

import Control.Lens ((&), (.~), (^.), (^..), (^?), (^@..))
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.ByteString.Lazy.Search
import Data.List (sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import qualified Data.Vector as V
import Network.Wreq
import Text.URI (renderStr)
import Text.URI.QQ (uri)

type CookieHeaderValue = BS.ByteString

pathEvaluatorUri =
  [uri|https://www.netflix.com/api/shakti/a468598e/pathEvaluator?withSize=false&materialize=true&isWatchlistEnabled=true|]

myListUri = [uri|https://www.netflix.com/browse/my-list|]

data NetflixTitle = NetflixTitle
  { titleName :: Text
  , titleType :: Text
  -- , titleCoverUrl :: Text
  } deriving (Eq, Show)

netflixMyListTitles :: CookieHeaderValue -> IO [NetflixTitle]
netflixMyListTitles cookie = do
  mlId <- myListId cookie
  r <- postWith opts (renderStr pathEvaluatorUri) (myListQuery mlId)
  return $ parseTitles (r ^. responseBody)
  where
    opts = defaults & header "Cookie" .~ [cookie]

parseTitles :: ByteString -> [NetflixTitle]
parseTitles jsonBS = mapMaybe toNetflixTitle originalOrderIds
  where
    Just json = jsonBS ^? key "value"
    list = head $ json ^.. key "lists" . members
    videos = mapMaybe (traverse (^? nth 1 . _String)) (list ^@.. members)
    originalOrderIds = map snd $ sortBy (comparing (decimal . fst)) videos
    toNetflixTitle id = do
      video <- json ^? key "videos" . key id
      title <- video ^? key "title" . _String
      tpe <- video ^? key "summary" . key "type" . _String
      return NetflixTitle {titleName = title, titleType = tpe}

myListId :: CookieHeaderValue -> IO Text
myListId cookie = do
  r <- getWith opts (renderStr myListUri)
  let bs = r ^. responseBody
      id = (decodeUtf8 . toStrict . findId) bs
  return id
  where
    opts = defaults & header "Cookie" .~ [cookie]
    token = "\"lists\":{\""
    findId = LC8.takeWhile (/= '"') . snd . breakAfter token

myListQuery :: Text -> Value
myListQuery id =
  let array vs = Array (V.fromList vs)
      range = object ["from" .= Number 0, "to" .= Number 200]
      myListPath = [String "lists", String id, range]
      myListSummary =
        array (myListPath ++ [array ["summary", "title", "userRating"]])
      myListBoxart = array (myListPath ++ ["boxarts", "_342x192", "webp"])
  in object ["paths" .= array [myListSummary, myListBoxart]]
