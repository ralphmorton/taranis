
module Backend.Web.Server (
  module Common.Web.Api,
  class Server,
  Handler,
  HandlerError(..),
  handle
) where

import Prelude (class Monad, bind, discard, pure, (&&), (<<<), (<=<), (=<<), (==))

import Control.Alt ((<|>))
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.MonadZero (guard)
import Data.Array (drop, head, null)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Simple.JSON (class ReadForeign, class WriteForeign, read_, write)

import Common.Web.Api

--

class Server m h f | h -> f where
  handle :: forall p. p h -> f -> Request -> Maybe (m Response)

--

type Handler = ExceptT HandlerError

--

data HandlerError = HandlerError {
  statusCode :: Int,
  errorMessage :: String
}

--

instance serverVerb :: (ReflectMethod v, WriteForeign a, Monad m) => Server m (Verb n v a) (ExceptT HandlerError m a) where
  handle _ f req = do
    let verbMethod = reflectMethod (Proxy :: Proxy v)
    let requestValid = req.method == verbMethod && null req.path
    guard requestValid
    (pure <<< verbResponse) (runExceptT f)

verbResponse :: forall m a. WriteForeign a => Monad m => m (Either HandlerError a) -> m Response
verbResponse m = do
  res <- m
  case res of
    Left (HandlerError { statusCode, errorMessage }) ->
      pure {
        statusCode,
        body: write errorMessage
      }
    Right a ->
      pure {
        statusCode: 200,
        body: write a
      }

--

instance serverLit :: (IsSymbol s, Server m b f) => Server m (Lit s :> b) f where
  handle _ f req = do
    pathPref <- head req.path
    guard (pathPref == reflectSymbol (SProxy :: SProxy s))
    handle (Proxy :: Proxy b) f (req { path = drop 1 req.path })

--

instance serverOr :: (Server m a f, Server m b g) => Server m (a :<|> b) (f :<|> g) where
  handle _ (Or f g) req = handle (Proxy :: Proxy a) f req <|> handle (Proxy :: Proxy b) g req

--

instance serverCapture :: (RequestParam a, Server m b f) => Server m (Capture a :> b) (a -> f) where
  handle _ f req = do
    a <- (fromRequestParam <=< head) req.path
    handle (Proxy :: Proxy b) (f a) (req { path = drop 1 req.path })

--

instance serverQueryParam :: (IsSymbol s, RequestParam a, Server m b f) => Server m (QueryParam s a :> b) (a -> f) where
  handle _ f req = do
    let label = reflectSymbol (SProxy :: SProxy s)
    a <- (fromRequestParam <=< Map.lookup label) req.queryParams
    handle (Proxy ::Proxy b) (f a) (req { queryParams = Map.delete label req.queryParams })

--

instance serverBody :: (ReadForeign a, Server m b f) => Server m (Body a :> b) (a -> f) where
  handle _ f req = do
    a <- read_ =<< req.body
    handle (Proxy :: Proxy b) (f a) (req { body = Nothing })

--

instance serverHeader :: (IsSymbol s, Server m b f) => Server m (Header s :> b) (String -> f) where
  handle _ f req = do
    let label = toLower (reflectSymbol (SProxy :: SProxy s))
    a <- (fromRequestParam <=< Map.lookup label) req.headers
    handle (Proxy :: Proxy b) (f a) (req { headers = Map.delete label req.headers })
