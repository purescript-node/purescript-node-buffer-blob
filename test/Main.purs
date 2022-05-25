module Test.Main where

import Prelude
import Effect (Effect)
import Test.Node.Buffer.Blob as Blob

main :: Effect Unit
main = Blob.test

