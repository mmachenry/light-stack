module LightStackTests exposing (tests)

import Test exposing (..)
import Expect exposing (..)

import LightStack exposing (..)
import Matrix

tests =
  describe "LightStack" [
    test "GoL on a 4x4 with 2x2 stable" <|
      \_ ->
        let b = Color 0 0 1
            c = Color 0 1 1
            m = Matrix.fromList [[b,b,b,b],[b,c,c,b],[b,c,c,b],[b,b,b,b]]
        in eval gol m |> Expect.equal m
    ]
