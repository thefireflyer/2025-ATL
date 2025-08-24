{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "lists"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "quickcheck"
  , "spec"
  , "spec-node"
  , "spec-quickcheck"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
