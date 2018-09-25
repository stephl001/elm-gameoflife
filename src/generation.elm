module Generation exposing (CellState(..), Column(..), Gen, Height(..), Row(..), Width(..), flatten, foldl, fromList, getDimensions, mapRowCells, mapRows, nextGen, repeat, toList, toggleCellState)

import Array exposing (Array)
import Array2D exposing (Array2D)


{-| Defines all possible states for a generation cell.
-}
type CellState
    = Dead
    | Alive


{-| Defines the height of a generation.
-}
type Height
    = Height Int


{-| Defines the width of a generation.
-}
type Width
    = Width Int


{-| Defines the zero-based index of a generation row.
-}
type Row
    = Row Int


{-| Defines the zero-based index of a generation column.
-}
type Column
    = Column Int


{-| Represents the main generation type. This is an opaque type so
you may only manipulate this type through the public functions of
this module.
-}
type Gen
    = Gen (Array2D CellState)


{-| Creates a generation with given height and width by repeating the same
state for each cell.

    someGen =
        repeat (Height 5) (Width 5) Alive

-}
repeat : Height -> Width -> CellState -> Gen
repeat (Height height) (Width width) state =
    Array2D.repeat height width state |> Gen


{-| Creates a generation from a list (row) of list of cell states (column).

    someGen =
        fromList [ [ Dead, Alive, Dead ], [ Alive, Dead, Alive ], [ Dead, Alive, Dead ] ]

-}
fromList : List (List CellState) -> Gen
fromList =
    Array2D.fromList >> Gen


{-| Converts a generation to its list representation. The resulting list will contain
one list for every row.
-}
toList : Gen -> List (List CellState)
toList gen =
    let
        cellsIdentity =
            mapRowCells (\_ _ -> identity)

        applyGen =
            (|>) gen
    in
    gen |> (mapRows cellsIdentity >> List.map applyGen)


{-| Gets the height and width of the given generation.
-}
getDimensions : Gen -> ( Height, Width )
getDimensions (Gen array) =
    ( Array2D.rows array |> Height, Array2D.columns array |> Width )


{-| Gets the state(dead or alive) of the requested cell given its row
and column indexes. Note that requesting the state of an invalid coordinate
will always yield CellState.Dead.
-}
getCellState : Row -> Column -> Gen -> CellState
getCellState (Row row) (Column col) (Gen array) =
    array |> Array2D.get row col |> Maybe.withDefault Dead


{-| Sets the state(dead or alive) of the requested cell given its row
and column indexes. Note that setting the state of a cell given an invalid
coordinate will yield the same generation.
-}
setCellState : Row -> Column -> CellState -> Gen -> Gen
setCellState (Row row) (Column col) state (Gen array) =
    array |> Array2D.set row col state |> Gen


{-| Toggles the state of the requested cell given its row and column indexes.
Note that setting the state of a cell given an invalid coordinate will yield
the same generation.
-}
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


{-| Gets the next generation by applying the rules of Conway's game of life on
every cells.
-}
nextGen : Gen -> Gen
nextGen ((Gen array) as gen) =
    array
        |> Array2D.indexedMap (\row col -> getCellNewState gen (Row row) (Column col))
        |> Gen


{-| Helper function that will call a callback function for every row in the
provided generation. The function will then produce a flat list of generated
elements. This function is helpfull for view rendering.

    tableRows = gen |> mapRows (_ -> tr [] [])

-}
mapRows : (Row -> a) -> Gen -> List a
mapRows f (Gen array) =
    let
        rows =
            array |> Array2D.rows
    in
    List.range 0 (rows - 1)
        |> List.map Row
        |> List.map f


{-| Helper function that will call a callback function for every cell in the
provided row given its index. The function will then produce a flat list of generated
elements. This function is helpfull for view rendering.

    tableCells = gen |> mapRowCells (_ _ _ -> td [] [])

-}
mapRowCells : (Row -> Column -> CellState -> a) -> Row -> Gen -> List a
mapRowCells f (Row row) (Gen array) =
    array
        |> Array2D.getRow row
        |> Maybe.withDefault Array.empty
        |> Array.indexedMap (\col -> f (Row row) (Column col))
        |> Array.toList


{-| Helper function that will call a callback function for every cell in the
provided generation starting with the upper left cell(0,0) up to the bottom right
cell(height-1,width-1). The function will then produce a flat list of generated
elements. This function is helpfull for view rendering.

    tableCells = gen |> flatten (_ _ _ -> div [] [])

-}
flatten : (Row -> Column -> CellState -> a) -> Gen -> List a
flatten f gen =
    gen
        |> mapRows (mapRowCells f)
        |> List.concatMap ((|>) gen)


{-| Reduce a generation from the top cell(0,0) to the bottom right
cell(height-1,width-1).

    aliveCellsCount = gen |> foldl (state count -> if state == Alive then count+1 else count) 0

-}
foldl : (CellState -> a -> a) -> a -> Gen -> a
foldl f init =
    flatten (\_ _ -> identity) >> List.foldl f init
