module RssMail.Convert (convertFeedToMail) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha512, integerDigest)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import qualified Data.Text.Lazy as Lazy
import Network.Mail.Mime (Mail(..), Address(..))
import Network.Mail.SMTP (simpleMail, htmlPart)
import Text.Feed.Query
import Text.Feed.Types (Feed(..), Item(..))

fromEmail = pack "noreply+feed@example.com"

-- | Format the entries of the feed as mail and attach an unique id to avoid
-- sending the same entry twice.
convertFeedToMail :: Address -> Feed -> [(Integer, Mail)]
convertFeedToMail to feed = map (convertEntryToMail to $ getFeedTitle feed) $ feedItems feed

-- | Format a single entry to a mail with attached id.
convertEntryToMail :: Address -> String -> Item -> (Integer, Mail)
convertEntryToMail to feedTitle entry =
    (entryId, simpleMail from [to] [] [] subject [htmlPart $ Lazy.pack html])
  where
    entryId = either (integerDigest . sha512 . fromString) (const 0) $ do
      maybe (Right "") (Left . snd) $ getItemId entry
      maybe (Right "") Left $ getItemTitle entry
    from = case getItemAuthor entry of
      Just x -> Address {
        addressName = Just $ pack x,
        addressEmail = fromEmail }
      Nothing -> Address {
        addressName = Just $ pack "Feed",
        addressEmail = fromEmail }
    subject = pack . subjectTemplate feedTitle $ fromMaybe "" $ getItemTitle entry
    subjectTemplate ft it = "[" ++ ft ++ "] " ++ it
    html = htmlContent ++ "\n\n" ++ htmlLink
    htmlContent = fromMaybe "" $ getItemSummary entry
    htmlLink = fromMaybe "" $ do
      link <- getItemLink entry
      return $ "<ul><li><a href=\"" ++ link ++ "\">[link]</a></li></ul>"
