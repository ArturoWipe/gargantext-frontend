{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "gargantext"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arraybuffer-types"
  , "arrays"
  , "bifunctors"
  , "colors"
  , "console"
  , "control"
  , "convertable-options"
  , "css"
  , "datetime"
  , "dom-filereader"
  , "dom-simple"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "ffi-simple"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "form-urlencoded"
  , "formula"
  , "functions"
  , "globals"
  , "graphql-client"
  , "http-methods"
  , "integers"
  , "js-timers"
  , "lists"
  , "markdown"
  , "markdown-smolder"
  , "math"
  , "maybe"
  , "media-types"
  , "milkis"
  , "newtype"
  , "nonempty"
  , "now"
  , "nullable"
  , "numbers"
  , "ordered-collections"
  , "orders"
  , "parallel"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "random"
  , "react"
  , "reactix"
  , "record"
  , "record-extra"
  , "routing"
  , "sequences"
  , "simple-json"
  , "simple-json-generics"
  , "simplecrypto"
  , "smolder"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "strings-extra"
  , "stringutils"
  , "these"
  , "toestand"
  , "transformers"
  , "tuples"
  , "tuples-native"
  , "typelevel"
  , "typelevel-prelude"
  , "uint"
  , "unfoldable"
  , "unsafe-coerce"
  , "uri"
  , "uuid"
  , "validation"
  , "web-file"
  , "web-html"
  , "web-storage"
  , "web-url"
  , "web-xhr"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
