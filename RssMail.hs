import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import RssMail.Config (configThread)
import RssMail.Seen
import RssMail.Send


-- | Run on configured feeds, send emails and update the database.
main = do
  seen <- MVar.newEmptyMVar
  config <- MVar.newEmptyMVar
  forkIO $ configThread config
  readSeen seen
  -- TODO(zombiecalypse) make full blown daemon.
  readFeeds config seen
