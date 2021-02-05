
module Test where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Debug.Trace
import Effect (Effect)

import Frontend.Web.Client

type Api =
  Lit "api" :>
    (
      Lit "foo" :> Capture Int :> Header "X-Test" :> Verb "getFoo" Get Int
      :<|>
      Lit "bar" :> QueryParam "x" Int :> Verb "postBar" Post Number
    )

baseRequest :: Request
baseRequest = {
  method: Get,
  path: [],
  queryParams: Map.empty,
  headers: Map.empty,
  body: Nothing
}

fooRequest :: Request
fooRequest =
  client
    (SProxy :: SProxy "getFoo")
    (Proxy :: Proxy Api)
    (123 :> "test-header" :> unit)
    baseRequest

barRequest :: Request
barRequest =
  client
    (SProxy :: SProxy "postBar")
    (Proxy :: Proxy Api)
    (321 :> unit)
    baseRequest

run :: Effect Unit
run = do
  traceM fooRequest
  traceM barRequest
