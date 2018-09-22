module GenerationTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Generation exposing (CellState(..), Gen, fromList, toList)
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
            genFromInt genHeight genWidth

        toggleAllCells : Gen -> Gen
        toggleAllCells gen =
            List.range 0 (genHeight * genWidth - 1)
                |> List.foldl (\i -> Generation.toggleCellState (i // genWidth) (i % genWidth)) gen
    in
    describe "The Generation module"
        [ describe "Create Generation"
            [ fuzz genFuzzer "Ensure we can construct and deconstruct a random generation" <|
                \someInt ->
                    let
                        randomGen =
                            genCreator someInt

                        fromToGen =
                            Generation.toList >> Generation.fromList

                        constructedGen =
                            randomGen |> fromToGen
                    in
                    Expect.equal randomGen constructedGen
            ]
        , describe "Generation Dimensions"
            [ fuzz2 (intRange 1 20) (intRange 1 20) "Ensure we can valid dimensions for a generation" <|
                \someHeight someWidth ->
                    let
                        randomGen =
                            Generation.repeat someHeight someWidth Dead

                        dim =
                            randomGen |> Generation.getDimensions
                    in
                    Expect.equal dim ( someHeight, someWidth )
            ]
        , describe "Generation Cell Toggling - All Cells Twice"
            [ fuzz genFuzzer "Toggling all cells of a random generation twice should yield the original generation" <|
                \someInt ->
                    let
                        randomGen =
                            genCreator someInt

                        toggleAllCellsTwice =
                            toggleAllCells >> toggleAllCells
                    in
                    Expect.equal randomGen (randomGen |> toggleAllCellsTwice)
            ]
        , describe "Generation Cell Toggling - All Cells Once"
            [ fuzz genFuzzer "Toggling all cells of a random generation once should yield a different generation" <|
                \someInt ->
                    let
                        randomGen =
                            genCreator someInt
                    in
                    Expect.notEqual randomGen (randomGen |> toggleAllCells)
            ]
        ]
