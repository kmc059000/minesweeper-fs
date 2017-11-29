namespace Coordinates

type GameSize = { Width: int; Height: int; }
    
type Coordinate = int

module Coordinates2 =
    let toIndex x y gameSize = x + y * gameSize.Width

    let toXY i gameSize =
        i / gameSize.Width, i % gameSize.Width, gameSize

    let private surroundingOffsets = 
        [(-1, -1);   (0, -1);  (1, -1);
         (-1,  0); (*(0, 0);*) (1, 0);
         (-1,  1);   (0, 1);   (1, 1);]
    
    let isValid gameSize coords =
        let inRange max x = x >= 0 && x < max
        let xInRange = inRange gameSize.Width
        let yInRange = inRange gameSize.Height
        let (x,y,_) = toXY coords gameSize
        (xInRange x) && (yInRange y)

    let getOffsetIndex gameSize coords offset =
        let (cx, cy, _) = toXY coords gameSize 
        let (dx, dy) = offset
        let x = cx + dx
        let y = cy + dy
        toIndex x y gameSize

    let getValidSurroundingCoordinates gameSize coords =
        surroundingOffsets
        |> Seq.map (getOffsetIndex gameSize coords)
        |> Seq.filter (isValid gameSize)
        |> Set.ofSeq

    let getValidSurroundingIndexes gameSize coords =
        coords 
        |> getValidSurroundingCoordinates gameSize

    let isNeighbor gameSize c1 c2 = 
        c1
        |> getValidSurroundingCoordinates gameSize
        |> Seq.contains c2
