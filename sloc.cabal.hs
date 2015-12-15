module Main where

import Cartel

main = defaultMain (pure (properties, [], [sloc]))
  where
    properties = blank
        { name         = "sloc"
        , version      = [0,1,0,0]
        , cabalVersion = Just (1, 8)
        , buildType    = Just simple
        }

    sloc = executable "sloc"
        [ mainIs       "Main.hs"
        , buildDepends deps
        , ghcOptions   ["-Wall", "-O2"]
        ]

deps = map unconstrained
    [ "base"
    , "bytestring"
    , "containers"
    , "directory"
    , "extra"
    , "filepath"
    , "foldl"
    , "streaming"
    , "unix"
    ]
