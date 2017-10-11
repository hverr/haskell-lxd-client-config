{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.LXD.Client.Config (
  -- * Files
  defaultFile
, parseDefaultFile
, parseFile

  -- * Types
, Config(..)
, Remote(..)
) where

import Control.Exception (throwIO)

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Yaml as Yaml

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

-- | Return the file path of the default configuration file.
defaultFile :: IO FilePath
defaultFile = do
    h <- getHomeDirectory
    return $ h </> ".config" </> "lxc" </> "config.yml"

-- | Parse the default configuration file
--
-- Throws an IO exception or a 'ParseException'
parseDefaultFile :: IO Config
parseDefaultFile = defaultFile >>= parseFile

-- | Parse the given configuration file.
--
-- Throws an IO exception or a 'ParseException'
parseFile :: FilePath -> IO Config
parseFile f = do
    r <- Yaml.decodeFileEither f
    case r of
        Left exc -> throwIO exc
        Right v -> return v

-- | The main configuration of the default LXD client.
--
-- Represents the file at @~\/.config\/lxc\/config.yml@.
data Config = Config {
    configDefaultRemote :: Text
  , configRemotes :: Map Text Remote
  , configAliases :: Map Text Text
  } deriving (Eq, Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> do
        configDefaultRemote <- v .: "default-remote"
        configRemotes       <- v .: "remotes"
        configAliases       <- v .: "aliases"
        return Config{..}

instance ToJSON Config where
    toJSON Config{..} = object [
        "default-remote" .= configDefaultRemote
      , "remotes"        .= configRemotes
      , "aliases"        .= configAliases
      ]

-- | The configuration of a remote.
--
-- Used by 'Config'.
data Remote = Remote {
    remoteAddr :: Text
  , remotePublic :: Bool
  , remoteProtocol :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON Remote where
    parseJSON = withObject "Remote" $ \v -> do
        remoteAddr     <- v .:  "addr"
        remotePublic   <- v .:  "public"
        remoteProtocol <- v .:? "protocol"
        return Remote{..}

instance ToJSON Remote where
    toJSON Remote{..} = object [
        "addr"     .= remoteAddr
      , "public"   .= remotePublic
      , "protocol" .= remoteProtocol
      ]
