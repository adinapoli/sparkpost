{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.SparkPost.V1.Transmission.Types where

import           Data.Char
import           Data.Time
import qualified Data.Text as T
import           Control.Lens
import           Control.Monad
import           Data.Monoid
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH

import           Network.API.SparkPost.V1.Types

data TransmissionOptions = TransmissionOptions {
    _tro_start_time :: Maybe SparkPostDate
    -- ^ Delay generation of messages until this datetime.
    -- Format YYYY-MM-DDTHH:MM:SS+-HH:MM or "now". Example: '2015-02-11T08:00:00-04:00'.
  , _tro_open_tracking :: Maybe Bool
    -- ^ Whether open tracking is enabled for this transmission.
  , _tro_click_tracking :: Maybe Bool
    -- ^ Whether click tracking is enabled for this transmission.
  , _tro_transactional :: Maybe Bool
    -- ^ Whether message is transactional or non-transactional for unsubscribe and suppression purposes.
  , _tro_sandbox :: Maybe Bool
    -- ^ Whether or not to use the sandbox sending domain.
  } deriving Show

deriveToJSON defaultOptions { fieldLabelModifier = drop 5, omitNothingFields = True } ''TransmissionOptions

data Recipient -- TODO
data Metadata -- TODO
data SubstitutionData -- TODO
data Content -- TODO

data Transmission = Transmission {
    _tra_options :: Maybe TransmissionOptions
    -- ^ JSON object in which transmission options are defined
  , _tra_recipients :: [Recipient]
    -- ^ Specify a stored recipient list or specify recipients inline.
  , _tra_campaign_id :: Maybe T.Text
    -- ^ Maximum length - 64 bytes
  , _tra_description :: Maybe T.Text
    -- ^ Maximum length - 1024 bytes
  , _tra_metadata :: Maybe Metadata
    -- ^ Metadata is available during events through the Webhooks and is provided to the substitution engine.
  , _tra_substitution_data :: Maybe SubstitutionData
    -- ^ Recipient substitution data takes precedence over transmission substitution data.
  , _tra_content :: Content
    -- ^ Specify a stored template or specify inline template content.
  } deriving Show

deriveToJSON defaultOptions { fieldLabelModifier = drop 5, omitNothingFields = True } ''Transmission
