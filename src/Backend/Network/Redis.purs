
module Backend.Network.Redis (
  ConfigProps,
  Config,
  RedisClient,
  create,
  createFromConnectionString,
  defaultConfig,
  disconnect,
  set,
  get,
  expire
) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row (class Nub, class Union)
import Record (merge)

--

foreign import data RedisClient :: Type

--

type ConfigProps = (
  port :: Int,
  host :: String,
  password :: Maybe String
)

type Config = Record ConfigProps

type CreateOptions = {
  password :: Nullable String
}

--

create :: forall a u m. MonadEffect m => Union a ConfigProps u => Nub u ConfigProps => Record a -> m RedisClient
create = liftEffect <<< create' <<< flip merge defaultConfig
  where create' cfg = create_ cfg.port cfg.host { password: toNullable cfg.password }

foreign import create_ :: Int -> String -> CreateOptions -> Effect RedisClient

--

createFromConnectionString :: forall m. MonadEffect m => String -> m RedisClient
createFromConnectionString = liftEffect <<< createFromConnectionString_

foreign import createFromConnectionString_ :: String -> Effect RedisClient

--

defaultConfig :: Config
defaultConfig = {
  port: 6379,
  host: "localhost",
  password: Nothing
}

--

disconnect :: forall m. MonadEffect m => RedisClient -> m Unit
disconnect = liftEffect <<< disconnect_

foreign import disconnect_ :: RedisClient -> Effect Unit

--

set :: forall m. MonadAff m => RedisClient -> String -> String -> m Unit
set client key val = (liftAff <<< fromEffectFnAff) (set_ client key val)

foreign import set_ :: RedisClient -> String -> String -> EffectFnAff Unit

--

get :: forall m. MonadAff m => RedisClient -> String -> m (Maybe String)
get client key = (liftAff <<< map toMaybe <<< fromEffectFnAff) (get_ client key)

foreign import get_ :: RedisClient -> String -> EffectFnAff (Nullable String)

--

expire :: forall m. MonadAff m => RedisClient -> String -> Int -> m Unit
expire client key ttl = (liftAff <<< fromEffectFnAff) (expire_ client key ttl)

foreign import expire_ :: RedisClient -> String -> Int -> EffectFnAff Unit
