
module Backend.Network.Azure.Storage (
  ServiceClient,
  ContainerClient,
  BlobClient,
  ConnectionString(..),
  ContainerName(..),
  BlobName(..),
  ListBlobsOptions,
  Blob,
  BlobSasUrlOptions,
  Tag,
  createServiceClient,
  createContainerClient,
  createBlobClient,
  upload,
  uploadFile,
  listBlobs,
  generateBlobSasUrl,
  setTags,
  getTags,
  deleteIfExists
) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect, liftEffect)

--

foreign import data ServiceClient :: Type
foreign import data ContainerClient :: Type
foreign import data BlobClient :: Type

--

newtype ConnectionString = ConnectionString String

derive instance eqConnectionString :: Eq ConnectionString
derive instance newtypeConnectionString :: Newtype ConnectionString _

--

newtype ContainerName = ContainerName String

derive instance eqContainerName :: Eq ContainerName
derive instance newtypeContainerName :: Newtype ContainerName _

--

newtype BlobName = BlobName String

derive instance eqBlobName :: Eq BlobName
derive instance newtypeBlobName :: Newtype BlobName _

--

type ListBlobsOptions = {
  prefix :: Maybe String
}

type RawListBlobsOptions = {
  prefix :: Nullable String
}

--

type Blob = {
  name :: BlobName
}

type RawBlob = {
  name :: String
}

--

type BlobSasUrlOptions = {
  ttlSeconds :: Int
}

--

type Tag = {
  name :: String,
  value :: String
}

--

createServiceClient :: forall m. MonadEffect m => ConnectionString -> m ServiceClient
createServiceClient = liftEffect <<< createServiceClient_ <<< unwrap

foreign import createServiceClient_ :: String -> Effect ServiceClient

--

createContainerClient :: forall m. MonadEffect m => ServiceClient -> ContainerName -> m ContainerClient
createContainerClient client = liftEffect <<< createContainerClient_ client <<< unwrap

foreign import createContainerClient_ :: ServiceClient -> String -> Effect ContainerClient

--

createBlobClient :: forall m. MonadEffect m => ContainerClient -> BlobName -> m BlobClient
createBlobClient client = liftEffect <<< createBlobClient_ client <<< unwrap

foreign import createBlobClient_ :: ContainerClient -> String -> Effect BlobClient

--

upload :: forall m. MonadAff m => BlobClient -> String -> m Unit
upload client str = (liftAff <<< fromEffectFnAff) (upload_ client str)

foreign import upload_ :: BlobClient -> String -> EffectFnAff Unit

--

uploadFile :: forall m. MonadAff m => BlobClient -> String -> m Unit
uploadFile client path = (liftAff <<< fromEffectFnAff) (uploadFile_ client path)

foreign import uploadFile_ :: BlobClient -> String -> EffectFnAff Unit

--

listBlobs :: forall m. MonadAff m => ContainerClient -> ListBlobsOptions -> m (Array Blob)
listBlobs client = map (map toBlob) <<< liftAff <<< fromEffectFnAff <<< listBlobs_ client <<< toRawOptions
  where
  toRawOptions opts = {
    prefix: toNullable opts.prefix
  }
  toBlob raw = {
    name: wrap raw.name
  }

foreign import listBlobs_ :: ContainerClient -> RawListBlobsOptions -> EffectFnAff (Array RawBlob)

--

generateBlobSasUrl :: forall m. MonadAff m => ContainerClient -> BlobSasUrlOptions -> Blob -> m String
generateBlobSasUrl client opts blob = do
  blobClient <- createBlobClient client blob.name
  (liftAff <<< fromEffectFnAff) (generateBlobSasUrl_ blobClient opts)

foreign import generateBlobSasUrl_ :: BlobClient -> BlobSasUrlOptions -> EffectFnAff String

--

setTags :: forall m. MonadAff m => BlobClient -> Array Tag -> m Unit
setTags client = liftAff <<< fromEffectFnAff <<< setTags_ client

foreign import setTags_ :: BlobClient -> Array Tag -> EffectFnAff Unit

--

getTags :: forall m. MonadAff m => BlobClient -> m (Array Tag)
getTags = liftAff <<< fromEffectFnAff <<< getTags_

foreign import getTags_ :: BlobClient -> EffectFnAff (Array Tag)

--

deleteIfExists :: forall m. MonadAff m => BlobClient -> m Unit
deleteIfExists = liftAff <<< fromEffectFnAff <<< deleteIfExists_

foreign import deleteIfExists_ :: BlobClient -> EffectFnAff Unit
