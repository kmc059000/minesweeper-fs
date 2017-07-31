module Common

open Minesweeper

    module Solvers = 
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
    

module Utilities =
    open Commands.Sweep
    open Commands.Flag
    open Solvers

    //i wish i could find a way to write these 3 functions as the same generic function
    let getExposedCell cell =
        match cell with 
        | Exposed e -> Some e
        | _ -> None
        
    let getFlaggedCell cell =
        match cell with 
        | Flagged e -> Some e
        | _ -> None

    let getHiddenCell cell =
        match cell with 
        | Hidden e -> Some e
        | _ -> None

    let getCellsOfType typeMatcher solution =
        solution.Cells |> Seq.choose typeMatcher

    let getFlaggedCells = getCellsOfType getFlaggedCell
    let getHiddenCells = getCellsOfType getHiddenCell
    let getExposedCells = getCellsOfType getExposedCell

    let getNeighborsOfType typeMatcher solution coords =
        getValidSurroundingIndexes solution.Game.Width solution.Game.Height coords
        |> Seq.map (fun n -> solution.Cells.[n.Index])
        |> Seq.choose typeMatcher
                
    let getFlaggedNeighbors = getNeighborsOfType getFlaggedCell
    let getHiddenNeighbors = getNeighborsOfType getHiddenCell
    let getExposedNeighbors = getNeighborsOfType getExposedCell

    let rec sweepAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            sweep game x.Coords.X x.Coords.Y
            |> sweepAll xs
        
    let rec flagAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            flag game x.Coords.X x.Coords.Y
            |> flagAll xs