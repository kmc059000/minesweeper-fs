module Minesweeper

type GameState = Start | Playing | Win | Dead | Quit

type CellState = Hidden | Exposed | Flagged
type Cell = {
    State: CellState;
    Index: int;
    IsMine: bool;
};

type Game = {
    Cells: Cell[];
    State: GameState;
    Width: int;
    Height: int;
    MineLocations: Set<int>;
    SecondaryMineLocations: Set<int>;
};

let getSurroundingCount (game:Game) (cell:Cell) =
    //todo find number of cells around this cell which has a mine
    5

let createGame (width:int) (height:int) (mineCount:int) (rand:System.Random) =
    let createCell (primaryMineLocations:Set<int>) index = {
        State = Hidden;
        Index = index;
        IsMine = primaryMineLocations.Contains index
    }

    let maxIndex = width * height
    
    let allMineLocations =
        Seq.initInfinite (fun _ -> rand.Next(maxIndex + 1))
        |> Seq.distinct
        |> Seq.take (mineCount + 1)
        |> List.ofSeq

    let primaryMineLocations = allMineLocations |> Seq.take (mineCount) |> Set.ofSeq
    let secondaryMineLocation = allMineLocations |> Seq.skip 1 |> Seq.take (mineCount) |> Set.ofSeq
    
    let cells = 
        [1..(width * height)] 
        |> Seq.map (createCell primaryMineLocations)
        |> Seq.toArray
    {
        Cells = cells;
        State = GameState.Start;
        Width = width;
        Height = height;
        MineLocations = primaryMineLocations;
        SecondaryMineLocations = secondaryMineLocation
    }

let createImpossibleSimpleGame<'a> = createGame 1 1 1
let createEasyGame<'a> = createGame 10 10 10
let createMediumGame<'a> = createGame 20 20 80
let createHardGame<'a> = createGame 30 30 400


