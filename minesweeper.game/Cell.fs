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

    
    let isValid coords =
        if coords.X >= 0 && coords.X < coords.GameSize.Width && coords.Y >= 0 && coords.Y < coords.GameSize.Height then true else false

   
module Cells =
    let create state coords isMine surroundingCount =
        { State = state; Coords = coords; IsMine = isMine; SurroundingCount = surroundingCount };

    let surroundingOffsets = 
        [(-1, -1);   (0, -1);  (1, -1);
         (-1,  0); (*(0, 0);*) (1, 0);
         (-1,  1);   (0, 1);   (1, 1);]

