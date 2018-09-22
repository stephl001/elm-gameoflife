module UtilitiesTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Generation exposing (CellState(..), Column(..), Gen, Height(..), Row(..), Width(..), fromList)
import Test exposing (..)
import TestUtilities exposing (..)


suite : Test
suite =
    let
        genCreator =
            genFromInt (Height 3) (Width 3)
    in
    describe "The TestUtilities module"
        [ describe "Create Generation"
            -- Nest as many descriptions as you like.
            [ test "9x9 All Alive From Integer" <|
                \_ ->
                    let
                        allAlive =
                            genCreator 511

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
                            genCreator 0

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
