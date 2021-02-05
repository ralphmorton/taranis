module Backend where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Effect (Effect)
import Effect.Aff (launchAff_)

import Backend.Arbiter (api, forkGarbageCollector)
import Backend.Context (bootstrap)
import Backend.Web.Fastify (fastify)

--

main :: Effect Unit
main = launchAff_ do
  ctx <- bootstrap
  runReaderT forkGarbageCollector ctx
  fastify
    { port: ctx.port, apiPrefix: "api", staticDirectory: pure "../frontend/" }
    (flip runReaderT ctx)
    api
