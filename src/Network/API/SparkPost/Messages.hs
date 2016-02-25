
module Network.API.SparkPost.Messages where

import           Network.API.SparkPost.Types
import           Network.API.SparkPost.Messages.Types
import           Network.API.SparkPost.Settings
import           Network.API.SparkPost.HTTP
import           Network.HTTP.Client
import           Data.Time
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | Send a new transactional message through SparkPost
send :: SparkPostKey
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

-- | Send a new transactional message through SparkPost using a template
sendTemplate :: SparkPostKey
             -- ^ The API key
             -> SparkPostTemplate
             -- ^ The template name
             -> [SparkPostTemplateContent]
             -- ^ Template content for 'editable regions'
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
sendTemplate k template content msg async ip_pool send_at = toSparkPostResponse MessagesSendTemplate (MessagesSendTemplateRq k template content msg async ip_pool send_at)
