{-# LANGUAGE OverloadedStrings #-}
module Network.API.SparkPost.V1.HTTP where

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid
import           Data.String.Conv
import qualified Data.Text as T
import           Network.API.SparkPost.V1.Settings
import           Network.API.SparkPost.V1.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types

toSparkPostResponse :: (SparkPostEndpoint ep, FromJSON a, ToJSON rq)
                    => ep
                    -> SparkPostAPIKey
                    -> rq
                    -> Maybe Manager
                    -> IO (SparkPostResponse a)
toSparkPostResponse ep apiKey rq mbMgr = do
  let fullUrl = sparkPostUrl <> toUrl ep
  rq' <- parseUrl (T.unpack fullUrl)
  let headers = [(hContentType, "application/json")
                ,(hAuthorization, toS apiKey)]
  let jsonBody = encode rq
  let req = rq' {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyLBS jsonBody
      }
  mgr <- maybe (newManager tlsManagerSettings) return mbMgr
  res <- responseBody <$> httpLbs req mgr
  case eitherDecode res of
    Left e ->  fail e
    Right v -> return v
