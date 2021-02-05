
module Frontend.Web.Client (
  module Common.Web.Api,
  class Client,
  client
) where

import Prelude (Unit, pure, (<>))

import Data.Map as Map
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Simple.JSON (class WriteForeign, write)

import Common.Web.Api

--

class Client (n :: Symbol) a f | n a -> f where
  client :: forall p q. p n -> q a -> f -> Request -> Request

--

instance clientOrLeft :: Client n a f => Client n (a :<|> b) f where
  client _ _ = client (SProxy :: SProxy n) (Proxy :: Proxy a)
else instance clientOrRight :: Client n b g => Client n (a :<|> b) g where
  client _ _ = client (SProxy :: SProxy n) (Proxy :: Proxy b)

--

instance clientVerb :: ReflectMethod v => Client n (Verb n v a) Unit where
  client _ _ _ req =
    req {
      method = reflectMethod (Proxy :: Proxy v)
    }

--

instance clientLit :: (IsSymbol s, Client n a f) => Client n (Lit s :> a) f where
  client _ _ f req =
    client
      (SProxy :: SProxy n)
      (Proxy :: Proxy a)
      f
      (req { path = req.path <> [reflectSymbol (SProxy :: SProxy s)] })

--

instance clientCapture :: (RequestParam c, Client n a f) => Client n (Capture c :> a) (c :> f) where
  client _ _ (Cons c f) req =
    client
      (SProxy :: SProxy n)
      (Proxy :: Proxy a)
      f
      (req { path = req.path <> [toRequestParam c] })

--

instance clientQueryParam :: (RequestParam c, IsSymbol s, Client n a f) => Client n (QueryParam s c :> a) (c :> f) where
  client _ _ (Cons c f) req =
    client
      (SProxy :: SProxy n)
      (Proxy :: Proxy a)
      f
      (req { queryParams = Map.insert (reflectSymbol (SProxy :: SProxy s)) (toRequestParam c) req.queryParams })

--

instance clientBody :: (WriteForeign b, Client n a f) => Client n (Body b :> a) (b :> f) where
  client _ _ (Cons b f) req =
    client
      (SProxy :: SProxy n)
      (Proxy :: Proxy a)
      f
      (req { body = pure (write b) })

--

instance clientHeader :: (IsSymbol s, Client n a f) => Client n (Header s :> a) (String :> f) where
  client _ _ (Cons c f) req =
    client
      (SProxy :: SProxy n)
      (Proxy :: Proxy a)
      f
      (req { headers = Map.insert (reflectSymbol (SProxy :: SProxy s)) c req.headers })
