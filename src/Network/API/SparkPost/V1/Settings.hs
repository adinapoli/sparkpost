{-# LANGUAGE OverloadedStrings #-}
module Network.API.SparkPost.V1.Settings where

import qualified Data.Text as T

sparkPostUrl :: T.Text
sparkPostUrl = "https://api.sparkpost.com/api/v1"

data SparkPostCalls =
  -- Transmissions API
    TransmissionCreate
  deriving Show

class SparkPostEndpoint ep where
  toUrl :: ep -> T.Text

instance SparkPostEndpoint SparkPostCalls where
  toUrl TransmissionCreate = "transmissions"
