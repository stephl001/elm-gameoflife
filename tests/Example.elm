module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Generation exposing (CellState(..), Gen, fromList)
import Test exposing (..)
import TestUtilities exposing (..)


suite : Test
suite =
    describe "The TestUtilities module"
        [ describe "Create Generation"
            -- Nest as many descriptions as you like.
            [ test "9x9 All Alive From Integer" <|
                \_ ->
                    let
                        allAlive =
                            genFromInt 3 3 511

                        expected =
                            Generation.fromList
                                [ [ Alive, Alive, Alive ]
                                , [ Alive, Alive, Alive ]
                                , [ Alive, Alive, Alive ]
                                ]
                    in
                    Expect.equal allAlive expected
            , test "9x9 All Dead From Integer" <|
                \_ ->
                    let
                        allAlive =
                            genFromInt 3 3 0

                        expected =
                            Generation.fromList
                                [ [ Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead ]
                                ]
                    in
                    Expect.equal allAlive expected
            ]
        ]
