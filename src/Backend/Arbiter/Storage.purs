
module Backend.Arbiter.Storage (
  storeTorrent,
  listTorrentBlobUrls,
  runGarbageCollectorDaemon
) where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (forever)
import Data.Array (find)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Number as Number
import Data.String (stripPrefix)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)

import Backend.Arbiter.Common (sha256Hex, cacheTtlSeconds, sentinelPrefix)
import Backend.Context (Context)
import Backend.Network.Azure.Storage as S
import Backend.Network.Redis as R
import Backend.Network.Torrent as T
import Backend.OS.Path as Path
import Common.Data.Base64Url as B64
import Common.Data.UUID (uuid)

--

storeTorrent :: Context -> T.Torrent -> Aff Unit
storeTorrent ctx@{ torrentClient, torrentDownloadPath } t = do
  uid <- uuid
  magnetUri <- T.magnetUri t
  magnetHash <- sha256Hex magnetUri
  let path = torrentDownloadPath <> "/" <> magnetHash
  files <- Path.listFilesRecursive path
  uploadSentinel ctx uid
  for_ files (uploadFile ctx uid path)
  indexTorrent ctx magnetHash uid
  deleteTorrent ctx t

uploadSentinel :: Context -> String -> Aff Unit
uploadSentinel { storageClient } uid = do
  let sentinelName = sentinelPrefix <> uid
  blobClient <- S.createBlobClient storageClient (wrap sentinelName)
  S.upload blobClient uid
  setAccessedTime blobClient

uploadFile :: Context -> String -> String -> String -> Aff Unit
uploadFile { storageClient } uid basePath filePath = do
  blobName <- (wrap <<< ((uid <> "-") <> _)) <$> B64.encode filePath
  blobClient <- S.createBlobClient storageClient blobName
  S.uploadFile blobClient (basePath <> "/" <> filePath)

indexTorrent :: Context -> String -> String -> Aff Unit
indexTorrent { redisClient } hash uid = do
  R.set redisClient hash uid
  R.expire redisClient hash cacheTtlSeconds

deleteTorrent :: Context -> T.Torrent -> Aff Unit
deleteTorrent { torrentClient } t = do
  magnetUri <- T.magnetUri t
  T.remove torrentClient { destroyStore: true } magnetUri

--

listTorrentBlobUrls :: forall m. MonadAff m => MonadAsk Context m => String -> m (Array { fileName :: String, url :: String })
listTorrentBlobUrls uid = do
  { storageClient } <- ask
  let prefix = uid <> "-"
  let sentinelName = sentinelPrefix <> uid
  sentinel <- S.createBlobClient storageClient (wrap sentinelName)
  setAccessedTime sentinel
  blobs <- S.listBlobs storageClient { prefix: pure prefix }
  for blobs \blob -> do
    let base64FileName = fromMaybe (unwrap blob.name) (stripPrefix (wrap prefix) (unwrap blob.name))
    fileName <- B64.decode base64FileName
    url <- S.generateBlobSasUrl storageClient { ttlSeconds: 86400 } blob
    pure { fileName, url }

--

runGarbageCollectorDaemon :: forall m. MonadAff m => MonadAsk Context m => m Unit
runGarbageCollectorDaemon = do
  { storageClient } <- ask
  liftAff $ forever do
    sentinels <- S.listBlobs storageClient { prefix: pure sentinelPrefix }
    traverse_ (gcCheckSentinel storageClient) sentinels
    delay (wrap 3600000.0)

gcCheckSentinel :: forall m. MonadAff m => S.ContainerClient -> S.Blob -> m Unit
gcCheckSentinel storageClient { name: sentinelName } = do
  let uid = fromMaybe (unwrap sentinelName) (stripPrefix (wrap sentinelPrefix) (unwrap sentinelName))
  let prefix = uid <> "-"
  sentinel <- S.createBlobClient storageClient sentinelName
  sentinelAccessTime <- getAccessedTime sentinel
  nowMs <- unInstant <$> liftEffect now
  case sentinelAccessTime of
    Just ms | unwrap nowMs - unwrap ms > 86400000.0 -> do
      blobs <- S.listBlobs storageClient { prefix: pure prefix }
      for_ blobs \blob -> do
        blobClient <- S.createBlobClient storageClient blob.name
        S.deleteIfExists blobClient
      S.deleteIfExists sentinel
    _ ->
      pure unit

--

setAccessedTime :: forall m. MonadAff m => S.BlobClient -> m Unit
setAccessedTime blobClient = do
  nowMs <- unwrap <<< unInstant <$> liftEffect now
  S.setTags blobClient [{ name: "accessed", value: show nowMs }]

getAccessedTime :: forall m. MonadAff m => S.BlobClient -> m (Maybe Milliseconds)
getAccessedTime blobClient = do
  mAccessed <- (map _.value <<< find ((_ == "accessed") <<< _.name)) <$> S.getTags blobClient
  (pure <<< map wrap) (Number.fromString =<< mAccessed)
