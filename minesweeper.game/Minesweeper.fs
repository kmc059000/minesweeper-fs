module Minesweeper

open Cells
open Cells.Coordinates
open Cells.Cells

type GameState = Start | Playing | Win | Dead | Quit | Exit


type Game = {
    CursorPosition: Coordinate;
    Cells: Map<int, Cell>;
    State: GameState;
    Width: int;
    Height: int;
    MineLocations: Set<int> option;
    MineCount: int;
    Seed: int;
    Random: System.Random;
};

let surroundingOffsets = 
    [(-1, -1);   (0, -1);  (1, -1);
     (-1,  0); (*(0, 0);*) (1, 0);
     (-1,  1);   (0, 1);   (1, 1);]

let getArrayIndex x y width = x + y * width

let getIndexOfOffset (coords:Coordinate) gameWidth (offset:int*int) =
    let (dx, dy) = offset
    let x = coords.X + dx
    let y = coords.Y + dy
    { X = x; Y = y; Index = (getArrayIndex x y gameWidth) }

let isValidCell w h coords =
    if coords.X >= 0 && coords.X < w && coords.Y >= 0 && coords.Y < h then true else false

let getCell game coords =
    game.Cells.[coords]

let getValidSurroundingIndexes gameWidth gameHeight coords =
    surroundingOffsets
    |> Seq.map (getIndexOfOffset coords gameWidth)
    |> Seq.filter (isValidCell gameWidth gameHeight)

let getValidSurroundingIndexesForCell gameWidth gameHeight cell =
    getValidSurroundingIndexes gameWidth gameHeight cell.Coords

let getSurroundingCount (mineLocations:Set<int>) gameWidth gameHeight (cell:Cell) =
    getValidSurroundingIndexesForCell gameWidth gameHeight cell
    |> Seq.filter (fun coords -> Set.contains coords.Index mineLocations)
    |> Seq.length

let createGame width height mineCount seed =
    let initCell index = {
        State = Hidden;
        Coords = { Index = index;
            X = index % width;
            Y = index / width;
        };        
        IsMine = false;
        SurroundingCount = None;
    }

    let cells = 
        [0..((width * height) - 1)] 
        |> Seq.map initCell
        |> Seq.map (fun c -> (c.Coords.Index,c))
        |> Map.ofSeq
    {
        CursorPosition = { X= 0; Y = 0; Index = 0; }
        Cells = cells;
        State = GameState.Start;
        Width = width;
        Height = height;
        MineLocations = None;
        MineCount = mineCount;
        Seed = seed;
        Random = new System.Random(seed);
    }

let createImpossibleSimpleGame<'a> = createGame 1 1 1
let createSweepGame<'a> = createGame 3 3 8
let createEasyGame<'a> = createGame 10 10 10
let createMediumGame<'a> = createGame 20 20 80
let createHardGame<'a> = createGame 30 30 400

let tryPlaceMine mineLocations cell =
    { cell with IsMine = Set.contains cell.Coords.Index mineLocations }

let setSurroundingCount game mineLocations cell =
    { cell with SurroundingCount = Some (getSurroundingCount mineLocations game.Width game.Height cell) }

let placeMines (game:Game) lastSelectedIndex = 
    let maxIndex = game.Width * game.Height
    
    let mineLocations =
        Seq.initInfinite  (fun i -> game.Random.Next(maxIndex))
        |> Seq.distinct
        //omit the first cell
        |> Seq.filter (fun c -> c <> lastSelectedIndex)
        |> Seq.take game.MineCount
        |> Set.ofSeq

    let newCells = 
        game.Cells
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map (tryPlaceMine mineLocations)
        |> Seq.map (setSurroundingCount game mineLocations)
        |> Seq.map (fun c -> (c.Coords.Index,c))
        |> Map.ofSeq
       
    { game with Cells = newCells; MineLocations = Some mineLocations; State = Playing }

let tryPlaceMines lastSelectedIndex (game:Game) =
    match game.State with
    | GameState.Start -> placeMines game lastSelectedIndex
    | _ -> game

let isWin (cells:Map<int, Cell>) = 
    cells
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.exists (fun x -> not x.IsMine && x.State <> CellState.Exposed)
    |> not

let testWin (game:Game) =
    let isWin = isWin game.Cells
    match isWin with
    | true -> { game with State = Win }
    | false -> game

let testLoss index (game:Game) = 
    let cell = game.Cells.[index]
    match cell.IsMine with
    | true -> { game with State = GameState.Dead; }
    | false -> game