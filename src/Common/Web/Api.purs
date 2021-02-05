
module Common.Web.Api where

import Prelude

import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (Foreign)

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

class RequestParam p where
  toRequestParam :: p -> String
  fromRequestParam :: String -> Maybe p

instance requestParamString :: RequestParam String where
  toRequestParam = identity
  fromRequestParam = pure <<< identity

instance requestParamInt :: RequestParam Int where
  toRequestParam = show
  fromRequestParam = Int.fromString

--

data Cons a b = Cons a b
infixr 5 type Cons as :>

cons :: forall a b. a -> b -> Cons a b
cons = Cons

infixr 5 cons as :>

--

data Or a b = Or a b
infixr 4 type Or as :<|>

or :: forall a b. a -> b -> Or a b
or = Or

infixr 4 or as :<|>

--

data Verb (n :: Symbol) v a

--

data Lit (s :: Symbol)

--

data Capture a

--

data QueryParam (s :: Symbol) a

--

data Body a

--

data Header (s :: Symbol)
