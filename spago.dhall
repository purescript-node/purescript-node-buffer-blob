{ name = "node-buffer-blob"
, dependencies =
  [ "aff-promise"
  , "arraybuffer-types"
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
}