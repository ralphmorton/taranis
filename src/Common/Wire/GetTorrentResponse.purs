
module Common.Wire.GetTorrentResponse (
  GetTorrentResponse(..),
  TorrentDirectory,
  TorrentFile,
  TorrentProgress
) where

import Prelude

import Control.Alt ((<|>))
import Simple.JSON (class ReadForeign, class WriteForeign, read', write)

--

data GetTorrentResponse
  = Available TorrentDirectory
  | Downloading TorrentProgress

type TorrentDirectory = {
  files :: Array TorrentFile
}

type TorrentFile = {
  fileName :: String,
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

instance readForeignGetTorrentResponse :: ReadForeign GetTorrentResponse where
  readImpl f = (Available <$> read' f) <|> (Downloading <$> read' f)
