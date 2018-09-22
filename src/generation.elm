module Generation exposing (CellState(..), Column(..), Gen, Height(..), Row(..), Width(..), flatten, foldl, fromList, getDimensions, mapRowCells, mapRows, nextGen, repeat, toList, toggleCellState)

import Array exposing (Array)
import Array2D exposing (Array2D)


type CellState
    = Dead
    | Alive


type Height
    = Height Int


type Width
    = Width Int


type Row
    = Row Int


type Column
    = Column Int


type Gen
    = Gen (Array2D CellState)


repeat : Height -> Width -> CellState -> Gen
repeat (Height height) (Width width) state =
    Array2D.repeat height width state |> Gen


fromList : List (List CellState) -> Gen
fromList =
    Array2D.fromList >> Gen


toList : Gen -> List (List CellState)
toList gen =
    let
        cellsIdentity =
            mapRowCells (\_ _ -> identity)

        applyGen =
            (|>) gen
    in
    gen |> (mapRows cellsIdentity >> List.map applyGen)


getDimensions : Gen -> ( Height, Width )
getDimensions (Gen array) =
    ( Array2D.rows array |> Height, Array2D.columns array |> Width )


getCellState : Row -> Column -> Gen -> CellState
getCellState (Row row) (Column col) (Gen array) =
    array |> Array2D.get row col |> Maybe.withDefault Dead


setCellState : Row -> Column -> CellState -> Gen -> Gen
setCellState (Row row) (Column col) state (Gen array) =
    array |> Array2D.set row col state |> Gen


toggleCellState : Row -> Column -> Gen -> Gen
toggleCellState row col gen =
    let
        newState =
            gen |> getCellState row col |> flipCellState
    in
    gen |> setCellState row col newState


flipCellState : CellState -> CellState
flipCellState state =
    if state == Dead then
        Alive

    else
        Dead


countAliveCells : List CellState -> Int
countAliveCells =
    let
        accumulateAlive state acc =
            if state == Alive then
                acc + 1

            else
                acc
    in
    List.foldl accumulateAlive 0


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


cellNeighbors : Row -> Column -> List ( Row, Column )
cellNeighbors (Row row) (Column col) =
    cartesian [ -1, 0, 1 ] [ -1, 0, 1 ]
        |> List.filter ((/=) ( 0, 0 ))
        |> List.map (\( deltaRow, deltaCol ) -> ( row + deltaRow |> Row, col + deltaCol |> Column ))


cellNeighborsStates : Row -> Column -> Gen -> List CellState
cellNeighborsStates row col gen =
    cellNeighbors row col |> List.map (\( r, c ) -> getCellState r c gen)


getCellNewState : Gen -> Row -> Column -> CellState -> CellState
getCellNewState gen row col currentState =
    cellNeighborsStates row col gen
        |> countAliveCells
        |> calculateNewState currentState


type alias AliveNeighbors =
    Int


calculateNewState : CellState -> AliveNeighbors -> CellState
calculateNewState currentstate aliveNeighbors =
    case ( currentstate, aliveNeighbors ) of
        ( Dead, 3 ) ->
            Alive

        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        _ ->
            Dead


nextGen : Gen -> Gen
nextGen ((Gen array) as gen) =
    array
        |> Array2D.indexedMap (\row col -> getCellNewState gen (Row row) (Column col))
        |> Gen


mapRows : (Row -> a) -> Gen -> List a
mapRows f (Gen array) =
    let
        rows =
            array |> Array2D.rows
    in
    List.range 0 (rows - 1)
        |> List.map Row
        |> List.map f


mapRowCells : (Row -> Column -> CellState -> a) -> Row -> Gen -> List a
mapRowCells f (Row row) (Gen array) =
    array
        |> Array2D.getRow row
        |> Maybe.withDefault Array.empty
        |> Array.indexedMap (\col -> f (Row row) (Column col))
        |> Array.toList


flatten : (Row -> Column -> CellState -> a) -> Gen -> List a
flatten f gen =
    gen
        |> mapRows (mapRowCells f)
        |> List.concatMap ((|>) gen)


foldl : (CellState -> a -> a) -> a -> Gen -> a
foldl f init =
    flatten (\_ _ -> identity) >> List.foldl f init
