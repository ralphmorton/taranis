
module Backend.Web.Fastify(
  Config,
  fastify
) where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String (toUpper)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import Simple.JSON (write)

import Backend.Web.Server (Method(..), Request, Response)

--

type Config = {
  port :: Int,
  apiPrefix :: String,
  staticDirectory :: Maybe String
}

--

foreign import data FReq :: Type
foreign import data FRsp :: Type

--

fastify :: forall m.
  MonadAff m =>
  Config ->
  (m ~> Aff) ->
  (Request -> Maybe (m Response)) -> Aff Unit
fastify { port, apiPrefix, staticDirectory } nat f = fromEffectFnAff (fastify_ port apiPrefix (toNullable staticDirectory) handler)
  where
  handler i o = launchAff_ do
    req <- toRequest i
    rsp <- flip catchError (pure <<< err500 <<< show) (fromMaybe (pure err404_) (nat <$> f req))
    liftEffect (writeResponse_ o rsp)

err404_ :: Response
err404_ = {
  statusCode: 404,
  body: write "Not found"
}

err500 :: String -> Response
err500 err = {
  statusCode: 500,
  body: write err
}

--

toRequest :: forall m. MonadEffect m => FReq -> m Request
toRequest i = liftEffect do
  method <- readMethod <$> requestMethod_ i
  path <- requestPath_ i
  queryParams <- toStringMap <$> requestQueryParams_ i
  headers <- toStringMap <$> requestHeaders_ i
  body <- toMaybe <$> requestBody_ i
  pure {
    method,
    path,
    queryParams,
    headers,
    body
  }

readMethod :: String -> Method
readMethod s = case toUpper s of
  "GET" -> Get
  "POST" -> Post
  "DELETE" -> Delete
  "PUT" -> Put
  "PATCH" -> Patch
  s' -> CustomVerb s'

toStringMap :: Array { key :: String, value :: String } -> Map.Map String String
toStringMap = foldr insert Map.empty
  where insert { key, value } = Map.insert key value

--

foreign import fastify_ :: Int -> String -> Nullable String -> (FReq -> FRsp -> Effect Unit) -> EffectFnAff Unit
foreign import requestMethod_ :: FReq -> Effect String
foreign import requestPath_ :: FReq -> Effect (Array String)
foreign import requestQueryParams_ :: FReq -> Effect (Array { key :: String, value :: String })
foreign import requestHeaders_ :: FReq -> Effect (Array { key :: String, value :: String })
foreign import requestBody_ :: FReq -> Effect (Nullable Foreign)
foreign import writeResponse_ :: FRsp -> Response -> Effect Unit
