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
    let getCell game coords = game.Cells.[coords]



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
    let createEasyGame = createGame 10 10 10
    let createMediumGame = createGame 20 20 80
    let createHardGame = createGame 30 30 400