module UtilitiesTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange)
import Generation exposing (CellState(..), Column(..), Gen, Height(..), Row(..), Width(..), fromList)
import Test exposing (..)
import TestUtilities exposing (..)


suite : Test
suite =
    let
        ( genHeight, genWidth ) =
            ( 5, 5 )

        genFuzzer =
            intRange 0 (2 ^ genHeight ^ genWidth - 1)

        genCreator =
            genFromInt (Height genHeight) (Width genWidth)

        genCreator3by3 =
            genFromInt (Height 3) (Width 3)
    in
    describe "The TestUtilities module"
        [ describe "Create Generation"
            -- Nest as many descriptions as you like.
            [ test "9x9 All Alive From Integer" <|
                \_ ->
                    let
                        allAlive =
                            genCreator3by3 511

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
                            genCreator3by3 0

                        expected =
                            Generation.fromList
                                [ [ Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead ]
                                ]
                    in
                    Expect.equal allAlive expected
            ]
        , describe "Rotate Generation"
            -- Nest as many descriptions as you like.
            [ fuzz genFuzzer "Rotating right 4 times should yield the original generation" <|
                \someInt ->
                    let
                        randomGen =
                            genCreator someInt

                        rotateRightFourTimes =
                            rotateGenRight >> rotateGenRight >> rotateGenRight >> rotateGenRight
                    in
                    Expect.equal randomGen (randomGen |> rotateRightFourTimes)
            , fuzz genFuzzer "Rotating left 4 times should yield the original generation" <|
                \someInt ->
                    let
                        randomGen =
                            genCreator someInt

                        rotateRightFourTimes =
                            rotateGenLeft >> rotateGenLeft >> rotateGenLeft >> rotateGenLeft
                    in
                    Expect.equal randomGen (randomGen |> rotateGenLeft)
            , fuzz genFuzzer "Rotating left twice should yield the same generation as rotating right twice" <|
                \someInt ->
                    let
                        randomGen =
                            genCreator someInt

                        rotateLeftTwice =
                            rotateGenLeft >> rotateGenLeft

                        rotateRightTwice =
                            rotateGenRight >> rotateGenRight
                    in
                    Expect.equal (randomGen |> rotateRightTwice) (randomGen |> rotateLeftTwice)
            ]
        ]
