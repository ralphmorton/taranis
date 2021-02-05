
module Backend.Context (
  Context,
  bootstrap
) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Process as P

import Backend.Network.Azure.Storage as S
import Backend.Network.Redis as R
import Backend.Network.Torrent as T

--

type Context = {
  port :: Int,
  redisClient :: R.RedisClient,
  torrentClient :: T.TorrentClient,
  storageClient :: S.ContainerClient,
  torrentDownloadPath :: String
}

--

bootstrap :: Aff Context
bootstrap = do
  port <- fromMaybe 8000 <<< Int.fromString <$> getEnv "PORT"
  redisClient <- bootstrapRedis
  torrentClient <- T.create
  storageClient <- bootstrapStorage
  torrentDownloadPath <- getEnv "TORRENT_DOWNLOAD_PATH"
  pure {
    port,
    redisClient,
    torrentClient,
    storageClient,
    torrentDownloadPath
  }

--

bootstrapRedis :: Aff R.RedisClient
bootstrapRedis = do
  mConnectionString <- lookupEnv "REDIS_URL"
  case mConnectionString of
    Just connectionString ->
      R.createFromConnectionString connectionString
    Nothing -> do
      port <- (Int.fromString =<< _) <$> lookupEnv "REDIS_PORT"
      host <- lookupEnv "REDIS_HOST"
      password <- lookupEnv "REDIS_PASSWORD"
      R.create R.defaultConfig {
        port = fromMaybe R.defaultConfig.port port,
        host = fromMaybe R.defaultConfig.host host,
        password = password
      }

--

bootstrapStorage :: Aff S.ContainerClient
bootstrapStorage = do
  connStr <- wrap <$> getEnv "AZURE_STORAGE_CONNECTIONSTRING"
  containerName <- wrap <$> getEnv "AZURE_STORAGE_CONTAINER_NAME"
  (flip S.createContainerClient containerName <=< S.createServiceClient) connStr

--

lookupEnv :: String -> Aff (Maybe String)
lookupEnv = liftEffect <<< P.lookupEnv

getEnv :: String -> Aff String
getEnv key = do
  let err = (throwError <<< error) ("Missing configuration: " <> key)
  maybe err pure =<< lookupEnv key
