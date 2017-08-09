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
        let inRange x max = x >= 0 && x < max
        if (inRange coords.X coords.GameSize.Width) && (inRange coords.Y coords.GameSize.Height) then true else false

    let getArrayIndex x y gameSize = x + y * gameSize.Width

    let getOffsetIndex coords offset =
        let (dx, dy) = offset
        let x = coords.X + dx
        let y = coords.Y + dy
        let index = getArrayIndex x y coords.GameSize
        {coords with X = x; Y = y; Index = index; }

    let getValidSurroundingCoordinates coords =
        surroundingOffsets
        |> Seq.map (getOffsetIndex coords)
        |> Seq.filter isValid
        |> Set.ofSeq

    let getValidSurroundingIndexes coords =
        coords 
        |> getValidSurroundingCoordinates 
        |> Set.map (fun c -> c.Index) 

    let isNeighbor c1 c2 = 
        c1
        |> getValidSurroundingCoordinates
        |> Seq.contains c2