
module Backend.Arbiter.Common (
  sha256Hex,
  cacheTtlSeconds,
  sentinelPrefix
) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Node.Crypto.Hash (Algorithm(SHA256), hex)

sha256Hex :: forall m. MonadEffect m => String -> m String
sha256Hex = liftEffect <<< hex SHA256

cacheTtlSeconds :: Int
cacheTtlSeconds = 86400

sentinelPrefix :: String
sentinelPrefix = "sentinel-"
