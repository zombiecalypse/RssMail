module RssMail.Seen (readSeen, saveSeen) where

import Control.Concurrent.MVar (putMVar)
import Data.Binary (encodeFile, decodeFileOrFail)
import qualified Data.Set as Set
import System.Directory (doesFileExist, getHomeDirectory)

readFileSeen :: IO (Set.Set Integer)
readFileSeen = do
  home <- getHomeDirectory
  exists <- doesFileExist $ home ++ "/.RssMail/seen.bin"
  if not exists
    then return Set.empty
    else do
      decoded <- decodeFileOrFail $ home ++ "/.RssMail/seen.bin"
      case decoded of
        Left _ -> do
          fail "Couldn't read seen feeds, file corrupted."
        Right x -> return x

readSeen seen = do
  fileSeen <- readFileSeen
  putMVar seen fileSeen

saveSeen seen newSet = do
  putMVar seen newSet
  home <- getHomeDirectory
  encodeFile (home ++ "/.RssMail/seen.bin") newSet
