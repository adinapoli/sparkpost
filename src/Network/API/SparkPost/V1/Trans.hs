{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.API.SparkPost.V1.Trans where

import Control.Applicative
import Control.Monad.Reader
import Network.API.SparkPost.V1.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS


--------------------------------------------------------------------------------
newtype SparkPostT m a = SparkPostT {
  runSparkPostT :: ReaderT (SparkPostKey, Manager) m a
  } deriving (MonadTrans, MonadReader (SparkPostKey, Manager),
              Functor, Applicative, Monad, MonadIO)


--------------------------------------------------------------------------------
type SparkPost = SparkPostT IO


--------------------------------------------------------------------------------
runSparkPost :: MonadIO m
             => SparkPostKey
             -> SparkPostT m a
             -> m a
runSparkPost key action = do
  mgr <- liftIO $ newManager tlsManagerSettings
  runReaderT (runSparkPostT action) (key, mgr)
