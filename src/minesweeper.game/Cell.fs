namespace Cells

open Coordinates

type CellState = Hidden | Exposed | Flagged

type Cell = {
    Coords: Coordinate;
    SurroundingCount: int option;
    TotalNeighbors: int;
};

module Cells =        
    let private getSurroundingCount gameSize mineLocations cell =
        cell.Coords
        |> Coordinates.getValidSurroundingIndexes gameSize
        |> Set.intersect mineLocations
        |> Seq.length

    let withSurroundingCount gameSize mineLocations cell =
        { cell with SurroundingCount = Some (getSurroundingCount gameSize mineLocations cell) }

    let getIndex c = Coordinates.toIndex c.Coords

module CellFactory =
    let initCell gameSize index =
        let totalNeighbors = Index index |> Coordinates.getValidSurroundingIndexes gameSize |> Seq.length

        {
            Coords = (Index index);
            SurroundingCount = None;
            TotalNeighbors = totalNeighbors; 
        }
    

