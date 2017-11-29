namespace Cells

open Coordinates

type CellState = Hidden | Exposed | Flagged

type Cell = {
    Coords: Coordinate2;
    SurroundingCount: int option;
    TotalNeighbors: int;
};

module Cells =        
    let private getSurroundingCount gameSize mineLocations cell =
        cell.Coords
        |> Coordinates2.getValidSurroundingIndexes gameSize
        |> Set.intersect mineLocations
        |> Seq.length

    let withSurroundingCount gameSize mineLocations cell =
        { cell with SurroundingCount = Some (getSurroundingCount gameSize mineLocations cell) }

    let getIndex c = Coordinates2.toIndex c.Coords

module CellFactory =
    let initCell gameSize index =
        let totalNeighbors = Index index |> Coordinates2.getValidSurroundingIndexes gameSize |> Seq.length

        {
            Coords = (Index index);
            SurroundingCount = None;
            TotalNeighbors = totalNeighbors; 
        }
    

