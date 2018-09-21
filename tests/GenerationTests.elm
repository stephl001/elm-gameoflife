module GenerationTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Generation exposing (CellState(..), Gen, fromList, toList)
import Test exposing (..)
import TestUtilities exposing (..)


suite : Test
suite =
    describe "The Generation module"
        [ describe "Create Generation"
            -- Nest as many descriptions as you like.
            [ fuzz (intRange 0 (2 ^ 5 ^ 5 - 1)) "Ensure we can construct and deconstruct a random generation" <|
                \someInt ->
                    let
                        randomGen =
                            genFromInt 5 5 someInt

                        fromToGen =
                            Generation.toList >> Generation.fromList

                        constructedGen =
                            randomGen |> fromToGen
                    in
                    Expect.equal randomGen constructedGen
            ]
        ]
