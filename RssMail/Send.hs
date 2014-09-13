module RssMail.Send (readFeeds) where
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forM_, void)
import qualified Data.Set as Set
import Network.Curl.Download (openAsFeed)
import Network.Mail.Mime (Mail(..), Address(..))
import Network.Mail.SMTP (renderSendMail)
import Text.Feed.Types (Feed(..))

import RssMail.Config
import RssMail.Convert
import RssMail.Seen

-- | converts a single feed into emails and sends them if necessary.
sendFeedAsMail :: MVar.MVar (Set.Set Integer) -> Address -> Feed -> IO ()
sendFeedAsMail seen address feed = do
  let mails = convertFeedToMail address feed
  -- We don't expect any other thread to produce the same hashes.
  seen' <- MVar.readMVar seen
  forM_ mails $ \(sha, mail) -> do
    if sha `Set.member` seen'
      then return () -- Ignore seen items
      else renderSendMail mail
  x <- MVar.takeMVar seen
  let newSet = Set.fromList (map fst mails) `Set.union` x
  void $ saveSeen seen newSet

readFeeds configMVar seenMVar = do
  config <- MVar.readMVar configMVar
  let to = configTo config
  forM_ (configFeeds config) $ \f -> do
    threadDelay 1000000
    putStrLn $ "Reading " ++ f
    s <- openAsFeed f
    case s of
      Left err -> putStrLn err
      Right feed -> sendFeedAsMail seenMVar to feed
