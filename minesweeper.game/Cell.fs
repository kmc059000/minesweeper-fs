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
        { State = state; Coords = coords; IsMine = isMine; SurroundingCount = surroundingCount };
        
    let getSurroundingCount mineLocations cell =
        Coordinates.getValidSurroundingIndexes cell.Coords
        |> Seq.filter (fun coords -> Set.contains coords.Index mineLocations)
        |> Seq.length

