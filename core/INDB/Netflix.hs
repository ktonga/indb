{-# LANGUAGE OverloadedStrings #-}

module INDB.Netflix where

import Control.Lens ((&), (.~), (^.), (^..))
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.ByteString.Lazy.Search
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Network.Wreq

type CookieHeaderValue = BS.ByteString

data NetflixTitle = NetflixTitle
  { title :: Text
  , coverUrl :: Text
  } deriving (Eq, Show)

netflixMyListTitles :: CookieHeaderValue -> IO [Value]
netflixMyListTitles cookie = do
  mlId <- myListId cookie
  r <- postWith opts url (myListQuery mlId)
  let bs = r ^. responseBody
  return (bs ^.. key "value" . key "videos" . members . key "title")
  where
    url =
      "https://www.netflix.com/api/shakti/a468598e/pathEvaluator" ++
      "?withSize=false&materialize=true&isWatchlistEnabled=true"
    opts = defaults & header "Cookie" .~ [cookie]

myListId :: CookieHeaderValue -> IO Text
myListId cookie = do
  r <- getWith opts "https://www.netflix.com/browse/my-list"
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
