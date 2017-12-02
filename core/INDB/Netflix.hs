{-# LANGUAGE OverloadedStrings #-}

module INDB.Netflix where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.ByteString.Lazy.Search
import Data.List (find)
import Data.List.Split (chop)
import Data.Maybe (fromMaybe)
import Data.Text (Text, strip, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Safe (headMay, tailSafe)
import Text.Taggy.Parser
import Text.Taggy.Types

type EMail = ByteString

type Password = ByteString

type CookieHeaderValue = BS.ByteString

data NetflixTitle = NetflixTitle
  { title :: Text
  , coverUrl :: Text
  } deriving (Eq, Show)

myListHtmlLogin :: EMail -> Password -> IO ByteString
myListHtmlLogin email password =
  Sess.withSession $ \sess -> do
    lr <- Sess.get sess "https://www.netflix.com/login"
    let html = lr ^. responseBody
    let authURL' = authURL html
    Sess.post sess "https://www.netflix.com/login" $ params authURL'
    r <- Sess.get sess "https://www.netflix.com/browse/my-list"
    return (r ^. responseBody)
  where
    authURL bs =
      let (_, after) = breakAfter "authURL\" value=\"" bs
      in LC8.takeWhile (/= '"') after
    params au =
      [ "email" := email
      , "password" := password
      , "rememberMe" := ("true" :: ByteString)
      , "flow" := ("websiteSignUp" :: ByteString)
      , "mode" := ("login" :: ByteString)
      , "action" := ("loginAction" :: ByteString)
      , "withFields" :=
        ("email,password,rememberMe,nextPage,showPassword" :: ByteString)
      , "authURL" := au
      , "nextPage" := ("" :: ByteString)
      , "showPassword" := ("" :: ByteString)
      ]

myListHtmlCookie :: CookieHeaderValue -> IO ByteString
myListHtmlCookie cookie =
  let r = getWith opts "https://www.netflix.com/browse/my-list"
  in fmap (^. responseBody) r
  where
    opts = defaults & header "Cookie" .~ [cookie]

parseTitles :: ByteString -> [NetflixTitle]
parseTitles htmlBS =
  let tags = taggyWith True $ decodeUtf8 htmlBS
  in chop nextTitle $ dropTilCover tags
  where
    nextTitle ts =
      let (Just nt) = do
            (TagOpen _ coverAttrs _) <- headMay ts
            cover <- attrValue <$> find ((== "style") . attrKey) coverAttrs
            (TagText title) <- find isTagText ts
            return (NetflixTitle (strip title) cover)
      in (nt, (dropTilCover . tailSafe) ts)
    dropTilCover = dropWhile (not . isCoverTag)
    isCoverTag (TagOpen "div" as _) =
      Attribute "class" "video-artwork" `elem` as
    isCoverTag _ = False
