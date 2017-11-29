namespace Cells

open Coordinates

type CellState = Hidden | Exposed | Flagged

type Cell = {
    Coords: Coordinate;
    Coords2: Coordinate2;
    SurroundingCount: int option;
    TotalNeighbors: int;
};

module Cells =        
    let private getSurroundingCount gameSize mineLocations cell =
        cell.Coords2
        |> Coordinates2.getValidSurroundingIndexes gameSize
        |> Set.intersect mineLocations
        |> Seq.length

    let withSurroundingCount gameSize mineLocations cell =
        { cell with SurroundingCount = Some (getSurroundingCount gameSize mineLocations cell) }

    let getIndex c = Coordinates2.toIndex c.Coords2

module CellFactory =
    let initCell gameSize index =
        let coords = 
            {
                Index = index
                X = index % gameSize.Width
                Y = index / gameSize.Width
                GameSize = gameSize
            
            }
        let totalNeighbors = coords |> Coordinates.getValidSurroundingIndexes |> Seq.length

        {
            Coords = coords;
            Coords2 = (Index index);
            SurroundingCount = None;
            TotalNeighbors = totalNeighbors; 
        }
    

