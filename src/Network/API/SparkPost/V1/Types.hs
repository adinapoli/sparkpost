{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.SparkPost.V1.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import           Data.Char
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TL
import qualified Data.Text.Lazy as TL
import           Data.Time
import           Data.Time.Format (TimeLocale, defaultTimeLocale)
import           Network.API.SparkPost.V1.Utils
import           Test.QuickCheck
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import           Text.Email.Validate


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
data SparkPostResponse k = SparkPostSuccess k
                         | SparkPostFailure SparkPostError
                         deriving Show

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
-- | Key value pair for replacing content in templates via 'Editable Regions'
data SparkPostTemplateContent = SparkPostTemplateContent {
    _mtc_name    :: T.Text
  , _mtc_content :: T.Text
  } deriving Show

makeLenses ''SparkPostTemplateContent
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''SparkPostTemplateContent

--------------------------------------------------------------------------------
type SparkPostAPIKey = T.Text

newtype SparkPostDate = SparkPostDate {
  fromSparkPostDate :: UTCTime
  } deriving Show

instance ToJSON SparkPostDate where
  toJSON = toJSON . fromSparkPostDate

instance FromJSON SparkPostDate where
  parseJSON = withText "SparkPostDate" $ \t ->
      case parseTimeM True defaultTimeLocale "%Y-%m-%d %I:%M:%S%Q" (T.unpack t) of
        Just d -> pure $ SparkPostDate d
        _      -> fail "could not parse SparkPost date"
