module RssMail.Seen (readSeen, saveSeen) where

import Control.Concurrent.MVar (putMVar)
import Data.Binary (encodeFile, decodeFileOrFail)
import qualified Data.Set as Set
import System.Directory (doesFileExist)

readFileSeen :: IO (Set.Set Integer)
readFileSeen = do
  exists <- doesFileExist "/home/aaron/.RssMail/seen.bin"
  if not exists
    then return Set.empty
    else do
      decoded <- decodeFileOrFail "/home/aaron/.RssMail/seen.bin"
      case decoded of
        Left _ -> do
          fail "Couldn't read seen feeds, file corrupted."
        Right x -> return x

readSeen seen = do
  fileSeen <- readFileSeen
  putMVar seen fileSeen

saveSeen seen newSet = do
  putMVar seen newSet
  encodeFile "/home/aaron/.RssMail/seen.bin" newSet
