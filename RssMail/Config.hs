{-# LANGUAGE TemplateHaskell #-}

module RssMail.Config( configThread, Config(..) ) where

import Control.Concurrent.MVar (putMVar, modifyMVar_)
import Control.Monad (forever)
import Data.Aeson (encode, decode)
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.Char (toLower)
import Data.Text (pack, unpack)
import Network.Mail.Mime (Address(..))

defaultAddress = Address {
  addressName = Nothing,
  addressEmail = pack "" }

instance Show Address where
  show (Address { addressName = Just n, addressEmail = mail }) = "\"" ++ unpack n ++ "\" <" ++ unpack mail ++ ">"
  show (Address { addressName = Nothing, addressEmail = mail }) = unpack mail

data Config = Config {
  configFeeds :: [String],
  configTo :: Address }
  deriving (Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = (drop 7) . map toLower }) ''Address)

$(deriveJSON (defaultOptions { fieldLabelModifier = (drop 6) . map toLower }) ''Config)

readoutConfig = do
  configFile <- readFile "/home/aaron/.RssMail/config.json"
  let conf = decode (fromString configFile) :: Maybe Config
  case conf of
         Just c -> return c
         Nothing -> do
           let c = Config {
             configFeeds = ["http://lambda-the-ultimate.org/rss.xml"],
             configTo = defaultAddress }
           writeFile "/home/aaron/.RssMail/config.json" . toString $ encode c
           return c

configThread config = do
  c <- readoutConfig
  putMVar config c
  forever $ do
    c <- readoutConfig
    modifyMVar_ config $ \_ -> return c
