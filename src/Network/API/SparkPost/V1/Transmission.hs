
module Network.API.SparkPost.V1.Transmission where

import qualified Data.Text as T
import           Data.Time
import           Network.API.SparkPost.V1.HTTP
import           Network.API.SparkPost.V1.Settings
import           Network.API.SparkPost.V1.Transmission.Types
import           Network.API.SparkPost.V1.Types
import           Network.HTTP.Client

--------------------------------------------------------------------------------
-- | Send a new transactional message through SparkPost.
send :: SparkPostAPIKey
     -- ^ The API key
     -> SparkPostMessage
     -- ^ The email message
     -> Maybe Bool
     -- ^ Enable a background sending mode that is optimized for bulk sending
     -> Maybe T.Text
     -- ^ ip_pool
     -> Maybe UTCTime
     -- ^ send_at
     -> Maybe Manager
     -> IO (SparkPostResponse [MessagesResponse])
send k msg async ip_pool send_at = toSparkPostResponse MessagesSend (MessagesSendRq k msg async ip_pool send_at)
