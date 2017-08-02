namespace Cells

type GameSize = { Width: int; Height: int; }

type Coordinate = {
    X: int;
    Y: int;
    Index: int;
    GameSize: GameSize;
};

type CellState = Hidden | Exposed | Flagged
type Cell = {
    State: CellState;
    Coords: Coordinate;
    IsMine: bool;
    SurroundingCount: int option;
};


module Coordinates =
    let create x y index size = 
        { X = x; Y = y; Index = index; GameSize = size; }


   
module Cells =
    let create state coords isMine surroundingCount =
        { State = state; Coords = coords; IsMine = isMine; SurroundingCount = surroundingCount };

    let surroundingOffsets = 
        [(-1, -1);   (0, -1);  (1, -1);
         (-1,  0); (*(0, 0);*) (1, 0);
         (-1,  1);   (0, 1);   (1, 1);]

