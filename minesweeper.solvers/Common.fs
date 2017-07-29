module Common

open Minesweeper
open Commands.Sweep
open Commands.Flag

type HiddenCell = { Coords: CellCoords; }
type ExposedCell = { Coords: CellCoords; SurroundingCount: int; }

type VisibleCell = 
    | Hidden of HiddenCell
    | Exposed of ExposedCell
    | Flagged of ExposedCell

//restricted view of the game for the solver
type SolutionState = Unsolved | Win | Dead
type Solution = {
    Game : Game;
    Cells: VisibleCell[];
    SolutionState: SolutionState;
}

let getSolutionFromGame (game:Game) =
    let getSolutionCell (c:Cell) = 
        match c.State with
        | CellState.Hidden -> Hidden { Coords = c.Coords; }
        | CellState.Exposed -> Exposed { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; }
        | CellState.Flagged -> Flagged { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; }

    let cells = game.Cells |> Array.map getSolutionCell
    let state = 
        match game.State with
        | GameState.Win -> Win
        | GameState.Dead -> Dead
        | GameState.Start | GameState.Playing | _ -> Unsolved
    { Game = game; Cells = cells; SolutionState = state; }

let solve solver (game:Game) =
    game |> getSolutionFromGame |> solver
    

let getUnsolvedCells game = 
    game.Cells 
    |> Seq.choose (fun c -> match c with Hidden h -> Some h | _ -> None)
