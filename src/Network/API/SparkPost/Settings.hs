{-# LANGUAGE OverloadedStrings #-}
module Network.API.SparkPost.Settings where

import qualified Data.Text as T

mandrillUrl :: T.Text
mandrillUrl = "https://mandrillapp.com/api/1.0/"

data SparkPostCalls =
  -- Users API
    UsersInfo
  | UsersPing
  | UsersPing2
  | UsersSenders
  -- Messages API
  | MessagesSend
  | MessagesSendTemplate
  | MessagesSearch deriving Show

class SparkPostEndpoint ep where
  toUrl :: ep -> T.Text

instance SparkPostEndpoint SparkPostCalls where
  toUrl UsersInfo = "users/info.json"
  toUrl UsersPing = "users/ping.json"
  toUrl UsersPing2 = "users/ping2.json"
  toUrl UsersSenders = "users/senders.json"
  toUrl MessagesSend = "messages/send.json"
  toUrl MessagesSendTemplate = "messages/send-template.json"
  toUrl MessagesSearch = "messages/search.json"
