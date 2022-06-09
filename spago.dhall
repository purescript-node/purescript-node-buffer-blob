{ name = "node-buffer-blob"
, dependencies =
  [ "aff-promise"
  , "arraybuffer-types"
  , "arrays"
  , "console"
  , "effect"
  , "maybe"
  , "media-types"
  , "newtype"
  , "node-buffer"
  , "nullable"
  , "prelude"
  , "web-streams"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository =
    "https://github.com/purescript-node/purescript-node-buffer-blob.git"
}
