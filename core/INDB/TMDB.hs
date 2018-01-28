{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric #-}

module INDB.TMDB where

import Network.Wreq
import Text.URI (renderStr)
import Text.URI.QQ (uri)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Control.Lens ((^.))
import GHC.Generics

data SearchResponse = SearchResponse {
                                      page :: Int
                                      , totalResults :: Int
                                      , totalPages :: Int
                                      }
                                      deriving (Show, Generic)

instance FromJSON SearchResponse where
    parseJSON = genericParseJSON defaultOptions {
                                                fieldLabelModifier = camelTo2 '_' }

searchMovieUri
  = [uri|https://api.themoviedb.org/3/search/movie?api_key=c7fc86d2b350de245860c184f9c455ae&query=Jack+Reacher|]

type Resp = Response SearchResponse

searchMovie :: IO SearchResponse
searchMovie = do
  r <- asJSON =<< get (renderStr searchMovieUri) :: IO Resp
  return (r ^. responseBody)
