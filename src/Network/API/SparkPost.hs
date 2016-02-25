{-| This package is an attempt to expose the SparkPost JSON API in pure Haskell.
    To do that, the library API comes in two flavours:

    * An IO-based, low-level 1:1 mapping of the JSON API,
      as described on <https://mandrillapp.com/api/docs/ the website>.
    * A handy monad transformer which can be plugged in your stack of choice.
-}

module Network.API.SparkPost (
    module M
  , sendEmail
  , sendTextEmail
  , emptyMessage
  , newTextMessage
  , newHtmlMessage
  , newTemplateMessage
  , liftIO

  -- * Appendix: Example Usage
  -- $exampleusage
  ) where

import Control.Monad.Reader
import Control.Lens
import Data.Time
import Text.Blaze.Html
import Network.API.SparkPost.Types as M
import Network.API.SparkPost.Messages as M
import Network.API.SparkPost.Messages.Types as M
import Network.API.SparkPost.Trans as M
import Data.Monoid
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as H

{- $exampleusage

The API was designed to allow to get you started as quickly as possible:

> {-# LANGUAGE OverloadedStrings #-}
> import Text.Email.Validate
> import Network.API.SparkPost
>
> main :: IO ()
> main = do
>   case validate "foo@example.com" of
>     Left err   -> print $ "Invalid email!" ++ show err
>     Right addr -> runSparkPost "MYTOKENHERE" $ do
>       let msg = "<p>My Html</p>"
>       res <- sendEmail (newTextMessage addr [addr] "Hello" msg)
>       case res of
>         SparkPostSuccess k -> liftIO (print k)
>         SparkPostFailure f -> liftIO (print f)

-}

--------------------------------------------------------------------------------
-- | Builds an empty message, given only the email of the sender and
-- the emails of the receiver. Please note that the "Subject" will be empty,
-- so you need to use either @newTextMessage@ or @newHtmlMessage@ to populate it.
emptyMessage :: EmailAddress -> [EmailAddress] -> SparkPostMessage
emptyMessage f t = SparkPostMessage {
   _mmsg_html = mempty
 , _mmsg_text = Nothing
 , _mmsg_subject = T.empty
 , _mmsg_from_email = (SparkPostEmail f)
 , _mmsg_from_name = Nothing
 , _mmsg_to = map newRecipient t
 , _mmsg_headers = H.empty
 , _mmsg_important = Nothing
 , _mmsg_track_opens = Nothing
 , _mmsg_track_clicks = Nothing
 , _mmsg_auto_text = Nothing
 , _mmsg_auto_html = Nothing
 , _mmsg_inline_css = Nothing
 , _mmsg_url_strip_qs = Nothing
 , _mmsg_preserve_recipients = Nothing
 , _mmsg_view_content_link = Nothing
 , _mmsg_bcc_address = Nothing
 , _mmsg_tracking_domain = Nothing
 , _mmsg_signing_domain = Nothing
 , _mmsg_return_path_domain = Nothing
 , _mmsg_merge = Nothing
 , _mmsg_global_merge_vars = []
 , _mmsg_merge_vars = []
 , _mmsg_tags = []
 , _mmsg_subaccount = Nothing
 , _mmsg_google_analytics_domains = []
 , _mmsg_google_analytics_campaign = Nothing
 , _mmsg_metadata = H.empty
 , _mmsg_recipient_metadata = []
 , _mmsg_attachments = []
 , _mmsg_images = []
  }


--------------------------------------------------------------------------------
-- | Create a new HTML message.
newHtmlMessage :: EmailAddress
               -- ^ Sender email
               -> [EmailAddress]
               -- ^ Receivers email
               -> T.Text
               -- ^ Subject
               -> Html
               -- ^ The HTML body
               -> SparkPostMessage
newHtmlMessage f t subj html = let body = mkSparkPostHtml html in
  ((mmsg_html .~ body) . (mmsg_subject .~ subj)) $ (emptyMessage f t)

--------------------------------------------------------------------------------
-- | Create a new template message (no HTML).
newTemplateMessage :: EmailAddress
                   -- ^ Sender email
                   -> [EmailAddress]
                   -- ^ Receivers email
                   -> T.Text
                   -- ^ Subject
                   -> SparkPostMessage
newTemplateMessage f t subj = (mmsg_subject .~ subj) $ (emptyMessage f t)

--------------------------------------------------------------------------------
-- | Create a new textual message. By default SparkPost doesn't require you
-- to specify the @mmsg_text@ when sending out the JSON Payload, and this
-- function ensure it will be present.
newTextMessage :: EmailAddress
               -- ^ Sender email
               -> [EmailAddress]
               -- ^ Receivers email
               -> T.Text
               -- ^ Subject
               -> T.Text
               -- ^ The body, as normal text.
               -> SparkPostMessage
newTextMessage f t subj txt = let body = unsafeMkSparkPostHtml txt in
  ((mmsg_html .~ body) .
   (mmsg_text .~ Just txt) .
   (mmsg_subject .~ subj)) (emptyMessage f t)


--------------------------------------------------------------------------------
-- | The simplest way to use the API. All you need to provide is a valid
-- 'SparkPostMessage' and this function will send an email inside a
-- 'SparkPostT' transformer. You are not forced to use the 'SparkPostT' context
-- though. Have a look at "Network.API.SparkPost.Messages" for an IO-based,
-- low lever function for sending email.
sendEmail :: MonadIO m
          => SparkPostMessage
          -> SparkPostT m (SparkPostResponse [MessagesResponse])
sendEmail msg = do
  (key, mgr) <- ask
  liftIO $ send key msg (Just True) Nothing Nothing (Just mgr)


--------------------------------------------------------------------------------
sendTextEmail :: MonadIO m
              => SparkPostMessage
              -> SparkPostT m (SparkPostResponse [MessagesResponse])
sendTextEmail msg = do
  (key, mgr) <- ask
  now <- liftIO getCurrentTime
  liftIO $ send key msg (Just True) Nothing (Just now) (Just mgr)
