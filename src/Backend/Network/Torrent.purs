
module Backend.Network.Torrent (
  TorrentClient,
  Torrent,
  MagnetUri,
  DownloadOptions,
  RemoveOptions,
  create,
  download,
  torrents,
  get,
  remove,
  progress,
  magnetUri
) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect, liftEffect)

--

foreign import data TorrentClient :: Type
foreign import data Torrent :: Type

--

type MagnetUri = String

type DownloadOptions = {
  path :: String
}

type RemoveOptions = {
  destroyStore :: Boolean
}

--

create :: forall m. MonadEffect m => m TorrentClient
create = liftEffect create_

foreign import create_ :: Effect TorrentClient

--

download :: forall m. MonadAff m => TorrentClient -> DownloadOptions -> MagnetUri -> m Torrent
download client opts uri = (liftAff <<< fromEffectFnAff) (download_ client opts uri)

foreign import download_ :: TorrentClient -> DownloadOptions -> MagnetUri -> EffectFnAff Torrent

--

torrents :: forall m. MonadEffect m => TorrentClient -> m (Array Torrent)
torrents = liftEffect <<< torrents_

foreign import torrents_ :: TorrentClient -> Effect (Array Torrent)

--

get :: forall m. MonadEffect m => TorrentClient -> MagnetUri -> m (Maybe Torrent)
get client uri = liftEffect (toMaybe <$> get_ client uri)

foreign import get_ :: TorrentClient -> MagnetUri -> Effect (Nullable Torrent)

--

remove :: forall m. MonadEffect m => TorrentClient -> RemoveOptions -> MagnetUri -> m Unit
remove client opts uri = liftEffect (remove_ client opts uri)

foreign import remove_ :: TorrentClient -> RemoveOptions -> MagnetUri -> Effect Unit

--

progress :: forall m. MonadEffect m => Torrent -> m Number
progress = liftEffect <<< progress_

foreign import progress_ :: Torrent -> Effect Number

--

magnetUri :: forall m. MonadEffect m => Torrent -> m MagnetUri
magnetUri = liftEffect <<< magnetUri_

foreign import magnetUri_ :: Torrent -> Effect MagnetUri
