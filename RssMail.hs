import qualified Network.Curl.Download as DL
import Data.Text (pack)
import qualified Data.Text.Lazy as Lazy
import Text.Feed.Query
import Text.Feed.Types (Feed(..), Item(..))
import Control.Monad (liftM, forM_)
import Data.Maybe (fromMaybe)
import Network.Mail.SMTP (simpleMail, htmlPart, renderSendMail)
import Network.Mail.Mime (Mail(..), Address(..))
import Data.ByteString.Lazy.UTF8 (toString)


data EntryId = EntryId String
  deriving (Ord, Eq, Show)

-- | Format the entries of the feed as mail and attach an unique id to avoid
-- sending the same entry twice.
convertFeedToMail :: Address -> Feed -> [(EntryId, Mail)]
convertFeedToMail to feed = map (convertEntryToMail to $ getFeedTitle feed) $ feedItems feed

-- | Format a single entry to a mail with attached id.
convertEntryToMail :: Address -> String -> Item -> (EntryId, Mail)
convertEntryToMail to feedTitle entry =
    (EntryId . maybe "" snd $ getItemId entry, simpleMail from [to] [] [] subject [htmlPart $ Lazy.pack html])
  where
    from = case getItemAuthor entry of
      Just x -> Address {
        addressName = Just $ pack x,
        addressEmail = pack "maergil+feed@gmail.com" }
      Nothing -> Address {
        addressName = Just $ pack "Feed",
        addressEmail = pack "maergil+feed@gmail.com" }
    subject = pack . subjectTemplate feedTitle $ fromMaybe "" $ getItemTitle entry
    subjectTemplate ft it = "[" ++ ft ++ "] " ++ it
    html = htmlContent ++ "\n\n" ++ htmlLink
    htmlContent = fromMaybe "" $ getItemSummary entry
    htmlLink = fromMaybe "" $ do
      link <- getItemLink entry
      return $ "<ul><li><a href=\"" ++ link ++ "\">[link]</a></li></ul>"

-- | converts a single feed into emails and sends them if necessary.
sendFeedAsMail address feed = do
  let mails = convertFeedToMail address feed
  forM_ (map snd mails) renderSendMail

toAddress = Address {
  addressName = Just $ pack "Aaron Karper",
  addressEmail = pack "maergil@gmail.com" }

smtpServer = "smtp.gmail.com"

-- | Run on configured feeds, send emails and update the database.
main = do
  s <- DL.openAsFeed "http://lambda-the-ultimate.org/rss.xml"
  case s of
    Left err -> fail err
    Right feed -> sendFeedAsMail toAddress feed
