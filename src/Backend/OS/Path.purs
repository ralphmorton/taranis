
module Backend.OS.Path (
  resolve,
  exists,
  listFilesRecursive
) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

--

resolve :: forall m. MonadEffect m => String -> m String
resolve = liftEffect <<< resolve_

foreign import resolve_ :: String -> Effect String

--

exists :: forall m. MonadEffect m => String -> m Boolean
exists = liftEffect <<< exists_

foreign import exists_ :: String -> Effect Boolean

--

listFilesRecursive :: forall m. MonadEffect m => String -> m (Array String)
listFilesRecursive = liftEffect <<< listFilesRecursive_

foreign import listFilesRecursive_ :: String -> Effect (Array String)
