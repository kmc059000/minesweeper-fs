namespace Cells

type Coordinate = {
    X: int;
    Y: int;
    Index: int;
};

type CellState = Hidden | Exposed | Flagged
type Cell = {
    State: CellState;
    Coords: Coordinate;
    IsMine: bool;
    SurroundingCount: int option;
};


module Coordinates =
    let create x y index = 
        { X = x; Y = y; Index = index; }


   
module Cells =
    let create state coords isMine surroundingCount =
        { State = state; Coords = coords; IsMine = isMine; SurroundingCount = surroundingCount };

