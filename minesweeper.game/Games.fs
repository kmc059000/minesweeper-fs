module Games

open Coordinates
open Cells
open Cells.Cells

type GameState = Start | Playing | Win | Dead | Quit | Exit

type Game = {
    CursorPosition: Coordinate;
    Cells: Map<int, Cell>;
    State: GameState;
    GameSize: GameSize;
    MineLocations: Set<int> option;
    MineCount: int;
    Seed: int;
    Random: System.Random;
};


module Game =
    let getCell game index = game.Cells.[index]

    let placeMines game lastSelectedIndex = 
        let tryPlaceMine mineLocations cell =
            { cell with IsMine = Set.contains cell.Coords.Index mineLocations }

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
            |> Seq.map (tryPlaceMine mineLocations)
            |> Seq.map (Cells.withSurroundingCount mineLocations)
            |> Seq.map (fun c -> (c.Coords.Index,c))
            |> Map.ofSeq
       
        { game with Cells = newCells; MineLocations = Some mineLocations; State = Playing }

    let tryPlaceMines lastSelectedIndex game =
        match game.State with
        | GameState.Start -> placeMines game lastSelectedIndex
        | _ -> game

    let isWin cells = 
        cells
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.exists (fun x -> not x.IsMine && x.State <> CellState.Exposed)
        |> not

    let testWin game =
        let isWin = isWin game.Cells
        match isWin with
        | true -> { game with State = Win }
        | false -> game

    let testLoss index game = 
        let cell = game.Cells.[index]
        match cell.IsMine with
        | true -> { game with State = GameState.Dead; }
        | false -> game




module GameFactory =
    let createGame width height mineCount seed =
        let gameSize = { Width = width; Height = height; }
        let initCell index = {
            State = Hidden;
            Coords = { Index = index;
                X = index % gameSize.Width;
                Y = index / gameSize.Width;
                GameSize = gameSize;
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
            CursorPosition = { X= 0; Y = 0; Index = 0; GameSize = gameSize; }
            Cells = cells;
            State = GameState.Start;
            GameSize = gameSize;
            MineLocations = None;
            MineCount = mineCount;
            Seed = seed;
            Random = new System.Random(seed);
        }

    let createImpossibleSimpleGame = createGame 1 1 1
    let createSweepGame = createGame 3 3 8
    let createEasyGame = createGame 8 8 10
    let createMediumGame = createGame 16 16 40
    let createHardGame = createGame 32 16 99