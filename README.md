# Conway's Game of Life in Elm

This package helps calculate generations based on the game of life generation rules.

This package will be used for a demonstration of elm language. You may find more information about game of life rules here:
[`Game of Life Rules`](https://en.wikipedia.org/wiki/Conway's_Game_of_Life#Rules)


## Example

The api is pretty straight forward. Once you have a valid generation instance, all you have to do is call ```nextGen``` to obtain the next generation. Using a lazy list, you could generate an infinite sequence of generations.

```elm
module MyModule exposing (runSimulation)

import Generation exposing (CellState(..), Column(..), Gen, Height(..), Row(..), Width(..), fromList, nextGen, toggleCellState)


runSimulation : () -> Gen
runSimulation () =
  let
    newGen = fromList 
              [
               [Dead,Dead,Dead]
              ,[Alive,Alive,Alive]
              ,[Dead,Dead,Dead]
              ]
  in
  newGen |> nextGen

```

There are many other functions to manipulate the generations but this remains a very simple package with an academic role.

Here are the available operations that can be performed:

  1. Create generation from list
  2. Generate list from a generation
  3. Create a generation with an identical repeted cell state
  4. Obtain next generation
  5. Map row, map cells and foldl for transformations
