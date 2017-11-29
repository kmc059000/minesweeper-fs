module Games

open Coordinates
open Cells
open Cells.Cells
open FSharpUtils

type GameState = Start | Playing | Win | Dead | Quit

type Game = {
    CursorPosition: Coordinate;
    Cells: Map<int, Cell>;
    State: GameState;
    GameSize: GameSize;
    FlaggedLocations: Set<int>;
    ExposedLocations: Set<int>;
    MineLocations: Set<int> option;
    MineCount: int;
    Seed: int;
    Random: System.Random;
    FlagCount: int;
    ExposedCount: int;
    CellCount: int;
};


module Game =
    let getCell game index = game.Cells.[index]
    let getCellState index game =
        let isExposed = game.ExposedLocations |> Set.contains index
        let isFlagged = game.FlaggedLocations |> Set.contains index
        match isExposed, isFlagged with
        | true, _ -> CellState.Exposed
        | _, true -> CellState.Flagged
        | _ -> CellState.Hidden

    let getCellStateFromCell cell game =
        getCellState (Cells.getIndex cell) game

    let setCellState index state game =
        let (flagAction, exposeAction) = 
            match state with
            | CellState.Exposed -> Set.remove, Set.add
            | CellState.Flagged -> Set.add, Set.remove
            | CellState.Hidden -> Set.remove, Set.remove
        { game with
            FlaggedLocations = game.FlaggedLocations |> flagAction index;
            ExposedLocations = game.ExposedLocations |> exposeAction index;
        }

    let setCellStateForCell cell state game =
        setCellState (Cells.getIndex cell) state game

    let isGameOfState state game cell =
        let index = Cells.getIndex cell
        let cellState = getCellState index game
        cellState  = state

    let isCellHidden = isGameOfState CellState.Hidden
    let isCellExposed = isGameOfState CellState.Exposed
    let isCellFlagged = isGameOfState CellState.Flagged
    let isCellMine index game = 
        match game.MineLocations with
        | Some s -> s |> Set.contains index
        | None -> false
    let isCellMineFromCell cell game = isCellMine (Cells.getIndex cell) game
    let isCellNotMine index game = not (isCellMine index game)
    let isCellNotMineFromCell cell game = isCellNotMine (Cells.getIndex cell) game

    let placeMines game lastSelectedIndex = 
        let maxIndex = game.GameSize.Width * game.GameSize.Height
    
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
            |> Seq.map (Cells.withSurroundingCount game.GameSize mineLocations)
            |> Seq.map (fun c -> (Cells.getIndex c,c))
            |> Map.ofSeq
       
        { game with Cells = newCells; MineLocations = Some mineLocations; State = Playing }

    let tryPlaceMines lastSelectedIndex game =
        match game.State with
        | GameState.Start -> placeMines game lastSelectedIndex
        | _ -> game

    let isWin game = 
        game.FlagCount = game.MineCount && game.ExposedCount + game.FlagCount = game.CellCount

    let testWin game =
        let isWin = isWin game
        match isWin with
        | true -> { game with State = Win }
        | false -> game

    let testLoss index game = 
        let isMine = isCellMine index game
        match isMine with
        | true -> { game with State = GameState.Dead; }
        | false -> game

    let getNeighborCells cell game =
        cell.Coords
        |> Coordinates.getValidSurroundingIndexes game.GameSize
        |> Seq.map (getCell game)

    let filterNeighborCells cell predicate game =
        cell.Coords
        |> Coordinates.getValidSurroundingIndexes game.GameSize
        |> Seq.map (getCell game)
        |> Seq.filter predicate

    let incrementExposedCount game = { game with ExposedCount = game.ExposedCount + 1 }
        

module GameFactory =
    let createGame width height mineCount seed =
        let gameSize = { Width = width; Height = height; }
        let cells = 
            [0..((width * height) - 1)] 
            |> Seq.map (CellFactory.initCell gameSize)
            |> Seq.map (fun c -> (Coordinates.toIndex c.Coords,c))
            |> Map.ofSeq
        {
            CursorPosition = Index 0;
            Cells = cells;
            State = GameState.Start;
            GameSize = gameSize;
            FlaggedLocations = Set.empty;
            ExposedLocations = Set.empty;
            MineLocations = None;
            MineCount = mineCount;
            FlagCount = 0;
            ExposedCount = 0;
            CellCount = width * height;
            Seed = seed;
            Random = new System.Random(seed);
        }

    let createImpossibleSimpleGame = createGame 1 1 1
    let createSweepGame = createGame 3 3 8
    let createEasyGame = createGame 8 8 10
    let createMediumGame = createGame 16 16 40
    let createHardGame = createGame 32 16 99