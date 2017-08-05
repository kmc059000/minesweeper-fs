module Common

open Coordinates
open Cells
open Games

    module Solvers = 
        type HiddenCell = { Coords: Coordinate; }
        type ExposedCell = { Coords: Coordinate; SurroundingCount: int; }

        type VisibleCell = 
            | Hidden of HiddenCell
            | Exposed of ExposedCell
            | Flagged of ExposedCell

        //restricted view of the game for the solver
        type SolutionState = Unsolved | Win | Dead
        type Solution = {
            Game : Game;
            Cells: Map<int, VisibleCell>;
            SolutionState: SolutionState;
        }

        let getSolutionFromGame (game:Game) =
            let getSolutionCell (c:Cell) = 
                match c.State with
                | CellState.Hidden -> (c.Coords.Index, Hidden { Coords = c.Coords; })
                | CellState.Exposed -> (c.Coords.Index, Exposed { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; })
                | CellState.Flagged -> (c.Coords.Index, Flagged { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; })

            let cells = game.Cells |> Map.toSeq |> Seq.map snd |> Seq.map getSolutionCell |> Map.ofSeq
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
        solution.Cells |> Map.toSeq |> Seq.map snd |> Seq.choose typeMatcher

    let solutionCellsOfSeq solution = solution.Cells |> Map.toSeq |> Seq.map snd

    let getFlaggedCells = getCellsOfType getFlaggedCell
    let getHiddenCells = getCellsOfType getHiddenCell
    let getExposedCells = getCellsOfType getExposedCell

    let getNeighborsOfType typeMatcher solution coords =
        Coordinates.getValidSurroundingIndexes coords
        |> Seq.map (fun n -> solution.Cells.[n.Index])
        |> Seq.choose typeMatcher
                
    let getFlaggedNeighbors = getNeighborsOfType getFlaggedCell
    let getHiddenNeighbors = getNeighborsOfType getHiddenCell
    let getExposedNeighbors = getNeighborsOfType getExposedCell

    let rec sweepAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            game
            |> sweep x.Coords.X x.Coords.Y
            |> sweepAll xs
        
    let rec flagAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            game |> flag x.Coords.X x.Coords.Y
            |> flagAll xs