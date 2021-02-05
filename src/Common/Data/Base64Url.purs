
module Common.Data.Base64Url (
  encode,
  decode
) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

--

encode :: forall m. MonadEffect m => String -> m String
encode = liftEffect <<< encode_

foreign import encode_ :: String -> Effect String

--

decode :: forall m. MonadEffect m => String -> m String
decode = liftEffect <<< decode_

foreign import decode_ :: String -> Effect String
