
module Network.API.SparkPost.Users where

import           Network.API.SparkPost.Settings
import           Network.API.SparkPost.HTTP
import           Network.API.SparkPost.Types
import           Network.API.SparkPost.Users.Types
import           Network.HTTP.Client


--------------------------------------------------------------------------------
-- | Return the information about the API-connected user
info :: SparkPostKey -> Maybe Manager -> IO (SparkPostResponse UsersInfoResponse)
info key = toSparkPostResponse UsersInfo (UsersRq key)


--------------------------------------------------------------------------------
-- | Validate an API key and respond to a ping
ping :: SparkPostKey -> Maybe Manager -> IO (SparkPostResponse UsersPingResponse)
ping _ _ = fail "users/ping.json doesn't return valid JSON, thus is not implemented yet."


--------------------------------------------------------------------------------
-- | Validate an API key and respond to a ping (anal JSON parser version)
ping2 :: SparkPostKey -> Maybe Manager -> IO (SparkPostResponse UsersPing2Response)
ping2 key = toSparkPostResponse UsersPing2 (UsersRq key)


--------------------------------------------------------------------------------
-- | Return the senders that have tried to use this account, both verified and unverified
senders :: SparkPostKey -> Maybe Manager -> IO (SparkPostResponse [UsersSendersResponse])
senders key = toSparkPostResponse UsersSenders (UsersRq key)
