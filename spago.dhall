{ name = "my-project"
, dependencies =
  [ "console", "effect", "foreign-object", "parsing", "prelude", "quickcheck" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
