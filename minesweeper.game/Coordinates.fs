namespace Coordinates

type GameSize = { Width: int; Height: int; }

type Coordinate = {
    X: int;
    Y: int;
    Index: int;
    GameSize: GameSize;
};


module Coordinates =
    let surroundingOffsets = 
        [(-1, -1);   (0, -1);  (1, -1);
         (-1,  0); (*(0, 0);*) (1, 0);
         (-1,  1);   (0, 1);   (1, 1);]

    let create x y index size = 
        { X = x; Y = y; Index = index; GameSize = size; }

    
    let isValid coords =
        if coords.X >= 0 && coords.X < coords.GameSize.Width && coords.Y >= 0 && coords.Y < coords.GameSize.Height then true else false

    let getArrayIndex x y gameSize = x + y * gameSize.Width

    let getOffsetIndex (coords:Coordinate) (offset:int*int) =
        let (dx, dy) = offset
        let x = coords.X + dx
        let y = coords.Y + dy
        let index = getArrayIndex x y coords.GameSize
        {coords with X = x; Y = y; Index = index; }

    let getValidSurroundingIndexes coords =
        surroundingOffsets
        |> Seq.map (getOffsetIndex coords)
        |> Seq.filter isValid

