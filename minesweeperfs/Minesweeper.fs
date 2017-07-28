module Minesweeper

type GameState = Start | Playing | Win | Dead | Quit | Exit

type CellCoords = {
    X: int;
    Y: int;
    Index: int;
};
type CellState = Hidden | Exposed | Flagged
type Cell = {
    State: CellState;
    Coords: CellCoords;
    IsMine: bool;
};

type Game = {
    Cells: Cell[];
    State: GameState;
    Width: int;
    Height: int;
    MineLocations: Set<int> option;
    MineCount: int;
    Randoms: int[];
};

let getArrayIndex x y width = x + y * width

let getIndexOfOffset (cell:Cell) (game:Game) (offset:int*int) =
    let (dx, dy) = offset
    let x = cell.Coords.X + dx
    let y = cell.Coords.Y + dy
    { X = x; Y = y; Index = (getArrayIndex x y game.Width) }

let isValidCell w h coords =
    if coords.X >= 0 && coords.X < w && coords.Y >= 0 && coords.Y < h then true else false

let getSurroundingCount (game:Game) (cell:Cell) =
    [(-1, -1);   (0, -1);  (1, -1);
     (-1,  0); (*(0, 0);*) (1, 0);
     (-1,  1);   (0, 1);   (1, 1);]
    |> Seq.map (getIndexOfOffset cell game)
    |> Seq.filter (isValidCell game.Width game.Height)
    |> Seq.map (fun coords -> game.Cells.[coords.Index])
    |> Seq.filter (fun c -> c.IsMine)
    |> Seq.length


let createGame width height mineCount randoms =
    let initCell index = {
        State = Hidden;
        Coords = { Index = index;
            X = index % width;
            Y = index / width;
        };        
        IsMine = false;
    }

    let cells = 
        [0..((width * height) - 1)] 
        |> Seq.map initCell
        |> Seq.toArray
    {
        Cells = cells;
        State = GameState.Start;
        Width = width;
        Height = height;
        MineLocations = None;
        MineCount = mineCount;
        Randoms = randoms;
    }

let createImpossibleSimpleGame<'a> = createGame 1 1 1
let createSweepGame<'a> = createGame 3 3 8
let createEasyGame<'a> = createGame 10 10 10
let createMediumGame<'a> = createGame 20 20 80
let createHardGame<'a> = createGame 30 30 400

let placeMines (game:Game) (firstSweepCell:Cell) = 
    let maxIndex = game.Width * game.Height
    
    let mineLocations =
        Seq.initInfinite (fun i -> game.Randoms.[i % game.Randoms.Length] % (maxIndex + 1))
        |> Seq.distinct
        //omit the first cell
        |> Seq.filter (fun c -> c <> firstSweepCell.Coords.Index)
        |> Seq.take game.MineCount
        |> Set.ofSeq

    let newCells =
        game.Cells
        |> Array.map (fun c -> {c with IsMine = Set.contains c.Coords.Index mineLocations})
    
    { game with Cells = newCells; MineLocations = Some mineLocations; State = Playing }

let tryPlaceMines (lastCell:Cell) (game:Game) =
    match game.State with
    | GameState.Start -> placeMines game lastCell
    | _ -> game
