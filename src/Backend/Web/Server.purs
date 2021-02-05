
module Backend.Web.Server where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.MonadZero (guard)
import Data.Array (drop, head, null)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (Foreign)
import Simple.JSON (class ReadForeign, class WriteForeign, read_, write)

--

data Proxy p = Proxy

--

data Method
  = Get
  | Post
  | Delete
  | Put
  | Patch
  | CustomVerb String

derive instance eqMethod :: Eq Method

data Get
data Post
data Delete
data Put
data Patch
data CustomVerb (s :: Symbol)

--

type Request = {
  method :: Method,
  path :: Array String,
  queryParams :: Map.Map String String,
  headers :: Map.Map String String,
  body :: Maybe Foreign
}

type Response = {
  statusCode :: Int,
  body :: Foreign
}

--

data Cons a b
infixr 5 type Cons as :>

data Or a b = Or a b
infixr 4 type Or as :<|>

or :: forall a b. a -> b -> Or a b
or = Or

infixr 4 or as :<|>

--

class ReflectMethod m where
  reflectMethod :: forall p. p m -> Method

instance reflectMethodGet :: ReflectMethod Get where
  reflectMethod _ = Get

instance reflectMethodPost :: ReflectMethod Post where
  reflectMethod _ = Post

instance reflectMethodDelete :: ReflectMethod Delete where
  reflectMethod _ = Delete

instance reflectMethodPut :: ReflectMethod Put where
  reflectMethod _ = Put

instance reflectMethodPatch :: ReflectMethod Patch where
  reflectMethod _ = Patch

instance reflectMethodCustom :: IsSymbol s => ReflectMethod (CustomVerb s) where
  reflectMethod _ = CustomVerb (reflectSymbol (SProxy :: SProxy s))

--

class FromRequestParam p where
  fromRequestParam :: String -> Maybe p

instance fromRequestParamString :: FromRequestParam String where
  fromRequestParam = pure <<< identity

instance fromRequestParamInt :: FromRequestParam Int where
  fromRequestParam = Int.fromString

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

data Verb v a

instance serverVerb :: (ReflectMethod v, WriteForeign a, Monad m) => Server m (Verb v a) (ExceptT HandlerError m a) where
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

data Lit (s :: Symbol)

instance serverLit :: (IsSymbol s, Server m b f) => Server m (Lit s :> b) f where
  handle _ f req = do
    pathPref <- head req.path
    guard (pathPref == reflectSymbol (SProxy :: SProxy s))
    handle (Proxy :: Proxy b) f (req { path = drop 1 req.path })

--

instance serverOr :: (Server m a f, Server m b g) => Server m (a :<|> b) (f :<|> g) where
  handle _ (Or f g) req = handle (Proxy :: Proxy a) f req <|> handle (Proxy :: Proxy b) g req

--

data Capture a

instance serverCapture :: (FromRequestParam a, Server m b f) => Server m (Capture a :> b) (a -> f) where
  handle _ f req = do
    a <- (fromRequestParam <=< head) req.path
    handle (Proxy :: Proxy b) (f a) (req { path = drop 1 req.path })

--

data QueryParam (s :: Symbol) a

instance serverQueryParam :: (IsSymbol s, FromRequestParam a, Server m b f) => Server m (QueryParam s a :> b) (a -> f) where
  handle _ f req = do
    let label = reflectSymbol (SProxy :: SProxy s)
    a <- (fromRequestParam <=< Map.lookup label) req.queryParams
    handle (Proxy ::Proxy b) (f a) (req { queryParams = Map.delete label req.queryParams })

--

data Body a

instance serverBody :: (ReadForeign a, Server m b f) => Server m (Body a :> b) (a -> f) where
  handle _ f req = do
    a <- read_ =<< req.body
    handle (Proxy :: Proxy b) (f a) (req { body = Nothing })

--

data Header (s :: Symbol)

instance serverHeader :: (IsSymbol s, Server m b f) => Server m (Header s :> b) (String -> f) where
  handle _ f req = do
    let label = toLower (reflectSymbol (SProxy :: SProxy s))
    a <- (fromRequestParam <=< Map.lookup label) req.headers
    handle (Proxy :: Proxy b) (f a) (req { headers = Map.delete label req.headers })
