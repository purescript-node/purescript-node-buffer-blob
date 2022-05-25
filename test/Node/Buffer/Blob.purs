module Test.Node.Buffer.Blob
  ( test
  ) where

import Prelude

import Control.Promise as Promise
import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common as MediaTypes
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Buffer.Blob as Blob
import Test.Assert (assertEqual)

test :: Effect Unit
test = launchAff_ do
  log "Testing fromString with no options"
  testFromStringNoOptions
  log "Testing fromString with options"
  testFromStringWithOptions
  log "Testing fromStrings with no options"
  testFromStringsNoOptions
  log "Testing fromStrings with options"
  testFromStringsWithOptions
  log "Testing fromArrayBuffer / toArrayBuffer"
  testToFromArrayBuffer
  log "Testing size"
  testSize
  log "Testing slice"
  testSlice
  where
  testFromStringNoOptions = do
    let
      expected = "hello world"
      blob = Blob.fromString expected Nothing
    actual <- Promise.toAffE $ Blob.text blob
    liftEffect $ assertEqual { actual, expected }

  testFromStringWithOptions = do
    let
      expected = "{\"hello\":\"world\"}"
      blob = Blob.fromString expected (Just { "type": MediaTypes.applicationJSON, endings: Blob.Transparent })
    actual <- Promise.toAffE $ Blob.text blob
    liftEffect $ assertEqual { actual, expected }

  testFromStringsNoOptions = do
    let
      input = [ "hello", "world" ]
      expected = fold input
      blob = Blob.fromStrings input Nothing
    actual <- Promise.toAffE $ Blob.text blob
    liftEffect $ assertEqual { actual, expected }

  testFromStringsWithOptions = do
    let
      input = [ "{\"hello\":\"world\"}", "{\"hola\":\"mundo\"}" ]
      expected = fold input
      blob = Blob.fromStrings input Nothing
    actual <- Promise.toAffE $ Blob.text blob
    liftEffect $ assertEqual { actual, expected }

  testToFromArrayBuffer = do
    let
      expected = "hello world"
      blob = Blob.fromString expected Nothing
    buffer <- Promise.toAffE $ Blob.toArrayBuffer blob
    actual <- Promise.toAffE $ Blob.text $ Blob.fromArrayBuffer buffer Nothing
    liftEffect $ assertEqual { actual, expected }

  testSize = do
    let
      expected = 11
      blob = Blob.fromString "hello world" Nothing
      actual = Blob.size blob
    liftEffect $ assertEqual { actual, expected }

  testSlice = do
    let
      expected = "ello wor"
      blob = Blob.fromString "hello world" Nothing
    actual <- Promise.toAffE $ Blob.text $ Blob.slice 1 9 blob
    liftEffect $ assertEqual { actual, expected }

