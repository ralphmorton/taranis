
module Backend.Arbiter.Api (
  api
) where

import Prelude

import Control.Monad.Fork.Class (class MonadFork, fork)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff)
import Simple.JSON (class WriteForeign, write)

import Backend.Arbiter.Common (sha256Hex, cacheTtlSeconds)
import Backend.Context (Context)
import Backend.Network.Redis as R
import Backend.Network.Torrent as T
import Backend.Web.Server (type (:>), Capture, Get, Handler, Lit, Proxy(..), Request, Response, Verb, handle)
import Common.Data.Base64Url as B64

--

data GetTorrentResponse
  = Available TorrentDirectory
  | Downloading TorrentProgress

type TorrentDirectory = {
  files :: Array TorrentAccessInfo
}

type TorrentAccessInfo = {
  url :: String
}

type TorrentProgress = {
  progress :: Number
}

instance writeForeignGetTorrentResponse :: WriteForeign GetTorrentResponse where
  writeImpl (Available dir) =
    write dir
  writeImpl (Downloading prog) =
    write prog

--

type Api = Lit "api" :> Lit "torrent" :> Capture String :> Verb Get GetTorrentResponse

--

api :: forall m.
  MonadAff m =>
  MonadFork Fiber m =>
  MonadAsk Context m =>
  Request ->
  Maybe (m Response)
api = handle (Proxy :: Proxy Api) server
  where
  server = requestTorrent

requestTorrent :: forall m.
  MonadAff m =>
  MonadFork Fiber m =>
  MonadAsk Context m =>
  String ->
  Handler m GetTorrentResponse
requestTorrent base64 = do
  magnetUri <- B64.decode base64
  mDir <- getCachedTorrent magnetUri
  case mDir of
    Just dir ->
      pure (Available dir)
    Nothing ->
      Downloading <$> lift (downloadTorrent magnetUri)

getCachedTorrent :: forall m.
  MonadAff m =>
  MonadAsk Context m =>
  String ->
  m (Maybe TorrentDirectory)
getCachedTorrent magnetHash = do
  { redisClient, storageClient } <- ask
  R.expire redisClient magnetHash cacheTtlSeconds
  mUid <- R.get redisClient magnetHash
  case mUid of
    Nothing ->
      pure Nothing
    Just uid ->
      (pure <<< pure) { files: [{ url: uid }] }

downloadTorrent :: forall m.
  MonadAff m =>
  MonadFork Fiber m =>
  MonadAsk Context m =>
  String ->
  m TorrentProgress
downloadTorrent magnetUri = do
  { torrentClient, torrentDownloadPath } <- ask
  mExisting <- T.get torrentClient magnetUri
  case mExisting of
    Just existing ->
      { progress: _ } <$> T.progress existing
    Nothing -> do
      magnetHash <- sha256Hex magnetUri
      let path = torrentDownloadPath <> "/" <> magnetHash
      (void <<< fork) (T.download torrentClient { path: path } magnetUri)
      pure {
        progress: 0.0
      }
