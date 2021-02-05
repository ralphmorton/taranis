{ name = "taranis"
, dependencies =
    [ "console"
    , "effect"
    , "aff-promise"
    , "psci-support"
    , "transformers"
    , "debug"
    , "nullable"
    , "fork"
    , "record"
    , "simple-json"
    , "node-process"
    , "crypto"
    , "now"
    , "numbers"
    , "halogen"
    , "simple-ajax"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/unit/**/*.purs" ]
}
