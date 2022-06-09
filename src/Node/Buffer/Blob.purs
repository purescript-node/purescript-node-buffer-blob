module Node.Buffer.Blob
  ( Blob
  , BlobEnding(..)
  , BlobOptions
  , fromArrayBuffers
  , fromBlobs
  , fromDataView
  , fromStrings
  , size
  , slice
  , stream
  , text
  , toArrayBuffer
  , tpe
  ) where

import Prelude

import Control.Promise (Promise)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, Uint8Array)
import Data.Maybe (Maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Node.Buffer (Offset)
import Web.Streams.ReadableStream (ReadableStream)

foreign import data Blob :: Type

data BlobEnding = Transparent | Native

type BlobOptions =
  { "type" :: MediaType
  , endings :: BlobEnding
  }

type BlobOptionsImpl =
  { "type" :: String
  , endings :: String
  }

toBlobOptionsImpl :: BlobOptions -> BlobOptionsImpl
toBlobOptionsImpl { "type": mediaType, endings } =
  { "type": un MediaType mediaType
  , endings: toEndings endings
  }
  where
  toEndings Transparent = "transparent"
  toEndings Native = "native"

foreign import fromStringsImpl :: NonEmptyArray String -> Nullable BlobOptionsImpl -> Blob

-- | Creates a new Blob from one or more strings
fromStrings :: NonEmptyArray String -> Maybe BlobOptions -> Blob
fromStrings strs opts = fromStringsImpl strs (opts <#> toBlobOptionsImpl # toNullable)

foreign import fromArrayBuffersImpl :: NonEmptyArray ArrayBuffer -> Nullable BlobOptionsImpl -> Blob

-- | Creates a new Blob from one ore more `ArrayBuffer`s
fromArrayBuffers :: NonEmptyArray ArrayBuffer -> Maybe BlobOptions -> Blob
fromArrayBuffers strs opts = fromArrayBuffersImpl strs (opts <#> toBlobOptionsImpl # toNullable)

foreign import fromDataViewImpl :: NonEmptyArray DataView -> Nullable BlobOptionsImpl -> Blob

-- | Creates a new Blob from one ore more `DataView`s
fromDataView :: NonEmptyArray DataView -> Maybe BlobOptions -> Blob
fromDataView strs opts = fromDataViewImpl strs (opts <#> toBlobOptionsImpl # toNullable)

foreign import fromBlobsImpl :: NonEmptyArray Blob -> Nullable BlobOptionsImpl -> Blob

-- | Creates a new Blob from one ore more `Blob`s
fromBlobs :: NonEmptyArray Blob -> Maybe BlobOptions -> Blob
fromBlobs strs opts = fromBlobsImpl strs (opts <#> toBlobOptionsImpl # toNullable)

-- | Copies the data in the Blob to a new JS ArrayBuffer
foreign import toArrayBuffer :: Blob -> Effect (Promise ArrayBuffer)

-- | Returns the size of the blob.
foreign import size :: Blob -> Int

-- | Creates and returns a new Blob containing a subset of this Blob objects data. The original Blob is not altered.
foreign import slice :: Offset -> Offset -> Blob -> Blob

-- | Returns a new ReadableStream that allows the content of the Blob to be read.
foreign import stream :: Blob -> Effect (ReadableStream Uint8Array)

-- | Returns a promise that fulfills with the contents of the Blob decoded as a UTF-8 string.
foreign import text :: Blob -> Effect (Promise String)

-- | Returns the content type of the blob
foreign import tpe :: Blob -> String
