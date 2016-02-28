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
  }

deriveToJSON defaultOptions { fieldLabelModifier = drop 5 } ''Transmission
