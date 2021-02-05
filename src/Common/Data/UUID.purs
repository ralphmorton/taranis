
module Common.Data.UUID (
  uuid
) where

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

--

uuid :: forall m. MonadEffect m => m String
uuid = liftEffect uuid_

foreign import uuid_ :: Effect String
