module TestUtilities exposing (genFromInt, rotateGenLeft, rotateGenRight)

import Bitwise exposing (and, shiftRightBy)
import Generation exposing (..)


mapHeads : List (List a) -> List a
mapHeads lists =
    case lists of
        (x :: _) :: ys ->
            x :: mapHeads ys

        _ ->
            []


mapTails : List (List a) -> List (List a)
mapTails lists =
    case lists of
        (_ :: xs) :: ys ->
            xs :: mapTails ys

        _ ->
            []


transpose : List (List a) -> List (List a)
transpose lists =
    case lists of
        (_ :: _) :: _ ->
            mapHeads lists :: (mapTails lists |> transpose)

        _ ->
            []


reverseRows : List (List a) -> List (List a)
reverseRows =
    List.map List.reverse


rotateRight : List (List a) -> List (List a)
rotateRight =
    transpose >> reverseRows


rotateLeft : List (List a) -> List (List a)
rotateLeft =
    rotateRight >> rotateRight >> rotateRight


genFromInt : Height -> Width -> Int -> Gen
genFromInt (Height height) (Width width) i =
    let
        stateFromBit r c =
            if (shiftRightBy (r * width + c) i |> and 1) == 1 then
                Alive

            else
                Dead
    in
    List.range 0 (height - 1)
        |> List.map (\r -> List.range 0 (width - 1) |> List.map (\c -> stateFromBit r c))
        |> Generation.fromList


rotateGenRight : Gen -> Gen
rotateGenRight =
    Generation.toList >> rotateRight >> Generation.fromList


rotateGenLeft : Gen -> Gen
rotateGenLeft =
    Generation.toList >> rotateLeft >> Generation.fromList
