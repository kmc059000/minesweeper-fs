namespace Cells

open Coordinates

type CellState = Hidden | Exposed | Flagged

type Cell = {
    State: CellState;
    Coords: Coordinate;
    IsMine: bool;
    SurroundingCount: int option;
};

module Cells =
    let create state coords isMine surroundingCount =
        { State = state; 
          Coords = coords; 
          IsMine = isMine; 
          SurroundingCount = surroundingCount };
        
    let private getSurroundingCount mineLocations cell =
        cell.Coords
        |> Coordinates.getValidSurroundingIndexes
        |> Set.intersect mineLocations
        |> Seq.length

    let withSurroundingCount mineLocations cell =
        { cell with SurroundingCount = Some (getSurroundingCount mineLocations cell) }
    
    

