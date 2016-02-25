{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.SparkPost.Types where

import           Network.API.SparkPost.Utils
import           Test.QuickCheck
import           Text.Email.Validate
import           Data.Char
import           Data.Maybe
import           Data.Time
import           Control.Applicative
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (TimeLocale, defaultTimeLocale)
#else
import System.Locale (TimeLocale, defaultTimeLocale)
#endif
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as TL
import qualified Data.Text.Lazy as TL
import           Control.Lens
import           Data.Monoid
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze


timeParse :: ParseTime t => TimeLocale -> String -> String -> Maybe t
#if MIN_VERSION_time(1,5,0)
timeParse = parseTimeM True
#else
timeParse = parseTime
#endif

--------------------------------------------------------------------------------
data SparkPostError = SparkPostError {
    _merr_status :: !T.Text
  , _merr_code :: !Int
  , _merr_name :: !T.Text
  , _merr_message :: !T.Text
  } deriving Show

makeLenses ''SparkPostError
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''SparkPostError


--------------------------------------------------------------------------------
data SparkPostEmailStatus = ES_Sent
                         | ES_Queued
                         | ES_Scheduled
                         | ES_Rejected
                         | ES_Invalid deriving Show

deriveJSON defaultOptions { constructorTagModifier = map toLower . drop 3 } ''SparkPostEmailStatus


--------------------------------------------------------------------------------
data SparkPostRejectReason = RR_HardBounce
                          | RR_SoftBounce
                          | RR_Spam
                          | RR_Unsub
                          | RR_Custom
                          | RR_InvalidSender
                          | RR_Invalid
                          | RR_TestModeLimit
                          | RR_Rule deriving Show

deriveJSON defaultOptions {
  constructorTagModifier = modRejectReason . drop 3
  } ''SparkPostRejectReason


--------------------------------------------------------------------------------
-- | The main datatypes which models the response from the SparkPost API,
-- which can be either a success or a failure.
data SparkPostResponse k =
    SparkPostSuccess k
  | SparkPostFailure SparkPostError deriving Show

instance FromJSON k => FromJSON (SparkPostResponse k) where
  parseJSON v = case (parseMaybe parseJSON v) :: Maybe k of
    Just r -> return $ SparkPostSuccess r
    Nothing -> do
    -- try to parse it as an error
      case (parseMaybe parseJSON v) :: Maybe SparkPostError of
        Just e -> return $ SparkPostFailure e
        Nothing -> fail $ show v <> " is neither a SparkPostSuccess or a SparkPostError."


--------------------------------------------------------------------------------
data SparkPostRecipientTag = To | Cc | Bcc deriving Show

deriveJSON defaultOptions { constructorTagModifier = map toLower } ''SparkPostRecipientTag


--------------------------------------------------------------------------------
newtype SparkPostEmail = SparkPostEmail EmailAddress deriving Show

instance ToJSON SparkPostEmail where
  toJSON (SparkPostEmail e) = String . TL.decodeUtf8 . toByteString $ e

instance FromJSON SparkPostEmail where
  parseJSON (String s) = case validate (TL.encodeUtf8 s) of
    Left err -> fail err
    Right v  -> return . SparkPostEmail $ v
  parseJSON o = typeMismatch "Expecting a String for SparkPostEmail." o


--------------------------------------------------------------------------------
-- | An array of recipient information.
data SparkPostRecipient = SparkPostRecipient {
    _mrec_email :: SparkPostEmail
    -- ^ The email address of the recipient
  , _mrec_name :: Maybe T.Text
    -- ^ The optional display name to use for the recipient
  , _mrec_type :: Maybe SparkPostRecipientTag
    -- ^ The header type to use for the recipient.
    --   defaults to "to" if not provided
  } deriving Show

makeLenses ''SparkPostRecipient
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''SparkPostRecipient

newRecipient :: EmailAddress -> SparkPostRecipient
newRecipient email = SparkPostRecipient (SparkPostEmail email) Nothing Nothing

instance Arbitrary SparkPostRecipient where
  arbitrary = pure SparkPostRecipient {
      _mrec_email = SparkPostEmail $ fromJust (emailAddress "test@example.com")
    , _mrec_name  =  Nothing
    , _mrec_type  =  Nothing
    }

--------------------------------------------------------------------------------
newtype SparkPostHtml = SparkPostHtml Blaze.Html

unsafeMkSparkPostHtml :: T.Text -> SparkPostHtml
unsafeMkSparkPostHtml = SparkPostHtml . Blaze.preEscapedToHtml

-- This might be slightly hairy because it violates
-- the nice encapsulation that newtypes offer.
mkSparkPostHtml :: Blaze.Html -> SparkPostHtml
mkSparkPostHtml = SparkPostHtml

instance Monoid SparkPostHtml where
  mempty = SparkPostHtml mempty
  mappend (SparkPostHtml m1) (SparkPostHtml m2) = SparkPostHtml (m1 <> m2)

instance Show SparkPostHtml where
  show (SparkPostHtml h) = show $ Blaze.renderHtml h

instance ToJSON SparkPostHtml where
  toJSON (SparkPostHtml h) = String . TL.toStrict . Blaze.renderHtml $ h

instance FromJSON SparkPostHtml where
  parseJSON (String h) = return $ SparkPostHtml (Blaze.preEscapedToHtml h)
  parseJSON v = typeMismatch "Expecting a String for SparkPostHtml" v

instance Arbitrary SparkPostHtml where
  arbitrary = pure $ mkSparkPostHtml "<p><b>FooBar</b></p>"

--------------------------------------------------------------------------------
type SparkPostTags = T.Text


--------------------------------------------------------------------------------
type SparkPostHeaders = Object


--------------------------------------------------------------------------------

data MergeVar = MergeVar {
      _mv_name    :: !T.Text
    , _mv_content :: Value
    } deriving Show

makeLenses ''MergeVar
deriveJSON defaultOptions { fieldLabelModifier = drop 4 } ''MergeVar

--------------------------------------------------------------------------------
data SparkPostMergeVars = SparkPostMergeVars {
    _mmvr_rcpt :: !T.Text
  , _mmvr_vars :: [MergeVar]
  } deriving Show

makeLenses ''SparkPostMergeVars
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''SparkPostMergeVars

--------------------------------------------------------------------------------
data SparkPostMetadata = SparkPostMetadata {
    _mmdt_rcpt :: !T.Text
  , _mmdt_values :: Object
  } deriving Show

makeLenses ''SparkPostMetadata
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''SparkPostMetadata


data Base64ByteString =
    EncodedB64BS B.ByteString
  -- ^ An already-encoded Base64 ByteString.
  | PlainBS B.ByteString
  -- ^ A plain Base64 ByteString which requires encoding.
  deriving Show

instance ToJSON Base64ByteString where
  toJSON (PlainBS bs)      = String . TL.decodeUtf8 . Base64.encode $ bs
  toJSON (EncodedB64BS bs) = String . TL.decodeUtf8 $ bs

instance FromJSON Base64ByteString where
  parseJSON (String v) = pure $ EncodedB64BS (TL.encodeUtf8 v)
  parseJSON rest = typeMismatch "Base64ByteString must be a String." rest

--------------------------------------------------------------------------------
data SparkPostWebContent = SparkPostWebContent {
    _mwct_type :: !T.Text
  , _mwct_name :: !T.Text
    -- ^ [for images] the Content ID of the image
    -- - use <img src="cid:THIS_VALUE"> to reference the image
    -- in your HTML content
  , _mwct_content :: !Base64ByteString
  } deriving Show

makeLenses ''SparkPostWebContent
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''SparkPostWebContent

--------------------------------------------------------------------------------
-- | The information on the message to send
data SparkPostMessage = SparkPostMessage {
   _mmsg_html :: SparkPostHtml
   -- ^ The full HTML content to be sent
 , _mmsg_text :: Maybe T.Text
   -- ^ Optional full text content to be sent
 , _mmsg_subject :: !T.Text
   -- ^ The message subject
 , _mmsg_from_email :: SparkPostEmail
   -- ^ The sender email address
 , _mmsg_from_name :: Maybe T.Text
   -- ^ Optional from name to be used
 , _mmsg_to :: [SparkPostRecipient]
   -- ^ A list of recipient information
 , _mmsg_headers :: SparkPostHeaders
   -- ^ optional extra headers to add to the message (most headers are allowed)
 , _mmsg_important :: Maybe Bool
   -- ^ whether or not this message is important, and should be delivered ahead
   -- of non-important messages
 , _mmsg_track_opens :: Maybe Bool
   -- ^ whether or not to turn on open tracking for the message
 , _mmsg_track_clicks :: Maybe Bool
   -- ^ whether or not to turn on click tracking for the message
 , _mmsg_auto_text :: Maybe Bool
   -- ^ whether or not to automatically generate a text part for messages that are not given text
 , _mmsg_auto_html :: Maybe Bool
   -- ^ whether or not to automatically generate an HTML part for messages that are not given HTML
 , _mmsg_inline_css :: Maybe Bool
   -- ^ whether or not to automatically inline all CSS styles provided in the message HTML
   -- - only for HTML documents less than 256KB in size
 , _mmsg_url_strip_qs :: Maybe Bool
   -- ^ whether or not to strip the query string from URLs when aggregating tracked URL data
 , _mmsg_preserve_recipients :: Maybe Bool
   -- ^ whether or not to expose all recipients in to "To" header for each email
 , _mmsg_view_content_link :: Maybe Bool
   -- ^ set to false to remove content logging for sensitive emails
 , _mmsg_bcc_address :: Maybe T.Text
   -- ^ an optional address to receive an exact copy of each recipient's email
 , _mmsg_tracking_domain :: Maybe T.Text
   -- ^ a custom domain to use for tracking opens and clicks instead of mandrillapp.com
 , _mmsg_signing_domain :: Maybe Bool
   -- ^ a custom domain to use for SPF/DKIM signing instead of mandrill
   -- (for "via" or "on behalf of" in email clients)
 , _mmsg_return_path_domain :: Maybe Bool
   -- ^ a custom domain to use for the messages's return-path
 , _mmsg_merge :: Maybe Bool
   -- ^ whether to evaluate merge tags in the message.
   -- Will automatically be set to true if either merge_vars
   -- or global_merge_vars are provided.
 , _mmsg_global_merge_vars :: [MergeVar]
   -- ^ global merge variables to use for all recipients. You can override these per recipient.
 , _mmsg_merge_vars :: [SparkPostMergeVars]
   -- ^ per-recipient merge variables, which override global merge variables with the same name.
 , _mmsg_tags :: [SparkPostTags]
   -- ^ an array of string to tag the message with. Stats are accumulated using tags,
   -- though we only store the first 100 we see, so this should not be unique
   -- or change frequently. Tags should be 50 characters or less.
   -- Any tags starting with an underscore are reserved for internal use
   -- and will cause errors.
 , _mmsg_subaccount :: Maybe T.Text
   -- ^ the unique id of a subaccount for this message
   -- - must already exist or will fail with an error
 , _mmsg_google_analytics_domains :: [T.Text]
   -- ^ an array of strings indicating for which any matching URLs
   -- will automatically have Google Analytics parameters appended
   -- to their query string automatically.
 , _mmsg_google_analytics_campaign :: Maybe T.Text
   -- ^ optional string indicating the value to set for the utm_campaign
   -- tracking parameter. If this isn't provided the email's from address
   -- will be used instead.
 , _mmsg_metadata :: Object
   -- ^ metadata an associative array of user metadata. SparkPost will store
   -- this metadata and make it available for retrieval.
   -- In addition, you can select up to 10 metadata fields to index
   -- and make searchable using the SparkPost search api.
 , _mmsg_recipient_metadata :: [SparkPostMetadata]
   -- ^ Per-recipient metadata that will override the global values
   -- specified in the metadata parameter.
 , _mmsg_attachments :: [SparkPostWebContent]
   -- ^ an array of supported attachments to add to the message
 , _mmsg_images :: [SparkPostWebContent]
   -- ^ an array of embedded images to add to the message
 } deriving Show

makeLenses ''SparkPostMessage
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''SparkPostMessage

instance Arbitrary SparkPostMessage where
  arbitrary = SparkPostMessage <$> arbitrary
                              <*> pure Nothing
                              <*> pure "Test Subject"
                              <*> pure (SparkPostEmail . fromJust $ emailAddress "sender@example.com")
                              <*> pure Nothing
                              <*> resize 2 arbitrary
                              <*> pure H.empty
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure []
                              <*> pure []
                              <*> pure []
                              <*> pure Nothing
                              <*> pure []
                              <*> pure Nothing
                              <*> pure H.empty
                              <*> pure []
                              <*> pure []
                              <*> pure []

--------------------------------------------------------------------------------
-- | Key value pair for replacing content in templates via 'Editable Regions'
data SparkPostTemplateContent = SparkPostTemplateContent {
    _mtc_name    :: T.Text
  , _mtc_content :: T.Text
  } deriving Show

makeLenses ''SparkPostTemplateContent
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''SparkPostTemplateContent

--------------------------------------------------------------------------------
type SparkPostKey = T.Text
type SparkPostTemplate = T.Text

newtype SparkPostDate = SparkPostDate {
  fromSparkPostDate :: UTCTime
  } deriving Show

instance ToJSON SparkPostDate where
  toJSON = toJSON . fromSparkPostDate

instance FromJSON SparkPostDate where
  parseJSON = withText "SparkPostDate" $ \t ->
      case timeParse defaultTimeLocale "%Y-%m-%d %I:%M:%S%Q" (T.unpack t) of
        Just d -> pure $ SparkPostDate d
        _      -> fail "could not parse SparkPost date"
