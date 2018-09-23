module GenerationTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Generation exposing (CellState(..), Column(..), Gen, Height(..), Row(..), Width(..), fromList, toList)
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

        toggleAllCells : Gen -> Gen
        toggleAllCells gen =
            List.range 0 (genHeight * genWidth - 1)
                |> List.foldl (\i -> Generation.toggleCellState (i // genWidth |> Row) (i % genWidth |> Column)) gen
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
                            Generation.repeat (Height someHeight) (Width someWidth) Dead

                        dim =
                            randomGen |> Generation.getDimensions
                    in
                    Expect.equal dim ( Height someHeight, Width someWidth )
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
        , describe "Generation with all cells alive"
            [ fuzz2 (intRange 3 20) (intRange 3 20) "Generation following a generation with only alive cells should only have corners alive" <|
                \someHeight someWidth ->
                    let
                        randomGen =
                            Generation.repeat (Height someHeight) (Width someWidth) Alive

                        hasOnlyDeadCells : Gen -> Bool
                        hasOnlyDeadCells =
                            Generation.foldl (\state acc -> acc && (state == Dead)) True
                    in
                    Expect.false "Generation should not be empty" (randomGen |> Generation.nextGen |> hasOnlyDeadCells)
            ]
        , describe "Next generation with rotation"
            [ fuzz genFuzzer "Rotating next generation must yield the same generation as rotating generation first and getting the next generation" <|
                \someInt ->
                    let
                        randomGen =
                            genCreator someInt

                        nextGenThenRotateRight =
                            Generation.nextGen >> rotateGenRight

                        rotateRightThenNextGen =
                            rotateGenRight >> Generation.nextGen
                    in
                    Expect.equal (nextGenThenRotateRight randomGen) (rotateRightThenNextGen randomGen)
            ]
        ]
